{
  Copyright 1998, 1999 Colin Davis

  This source code is free and you may use it any way you wish.  Use
  at your own risk.  I make no warranty that the  code is correct.

Compile with TMT Pascal for DOS/DPMI with the TMTStub (others may work).
You can get the evaluation version from http://www.tmt.com.  It will
compile this and is a fully working  compiler.

This unit provides the Pascal programmer with the basic disk functions
like the biosdisk() function in BIOS.H from DJGPP and other C libraries.
In addition it provides many other disk functions like LBA to CHS/ CHS
to LBA translation and functions to access disks using either addressing
mode.  It can use the int13h extensions to access a drive larger than 8.4GB.

I put in an object type TpartitionTable because the partitions aren't
part of an OS and if you're using a disk library chances are you want to
read or write the partition table(s).  The other source code on this
site provides examples of how to use this object and this unit in general.

To make your program not crash immediately when using Pdisk32, call
InitDOSmemBuffer(), then use GetDrvParams(), then before terminating the
program call RemoveDOSmemBuffer().  Example:

program x;
uses pdisk32;

  program stuff . . .

begin
  initdosmembuffer;
  getdrvparams;
  // call your program stuff here
  removedosmembuffer;

end.



  With small changes you can make this work on Free Pascal for DOS Go32.
  You could convert it to Free Pascal for Linux and you could get rid
  of about half of the code in the process.   For Go32 you need to change
  the parts that move memory to/from DOS memory and the rmintr() function
  is called something else but that's about it.  Consult the documentation
  on the Go32 unit and other DPMI things.


}

unit pdisk32;

interface
uses dos,go32,smallbuf,crt;

const

{ Partition table offsets in the mbr }
  part1 = 446;
  part2 = part1 + 16;
  part3 = part1 + 32;
  part4 = part1 + 48;

  FAT12 = 1;   { on floppies or small hard disks }
  fat16 = 6;
  EXT = 5;  {extended partition table }
  EXT95=15; {extended partition table using LBA}
  FAT32 = 12; {FAT32 using LBA in the partition table addresses}
  FAT32x = 11;
  WIN95 = 14; {FAT16 -- Use LBA in the addresses instead of CHS }
  DOS4 = 4; {FAT16 }
  NTFS = 7;
  DBFS = $E0;

  PRI = 0;
  BR = 0;
  PART = 1;

  sectorsize = 512;

{BIOS numbers for the physical drives.}
  Drive_A = $00;
  drive_B=  $01;
  drive_1 = $80;
  drive_2 = $81;
  drive_3 = $82;
  drive_4 = $83;

{Disk error codes returned from BIOS disk operations}
  noerror = 0;
  badcommand = 1;
  badsector = 2;
  writeprotected = 3;
  sectornotfound = 4;
  diskchange = 6;
  invalidmedia = $0C;
  controllererror = $20;
  seekfailure = $40;
  timeout = $80;

  NotFAT16 = $FE;
  NoDPB = $FF;
  NOTEXT = $FD;
  NOFREEREC = $FC;


  floppies=[drive_A,drive_B];
  harddisks=[drive_1,drive_2,drive_3,drive_4];


  FATtypes = [FAT12,DOS4,FAT16,WIN95,FAT32x,FAT32];
  EXTtypes = [EXT,EXT95];
  FAT16types = [DOS4,WIN95,FAT16];
  FAT32types = [FAT32x,FAT32];

{From compatibility with TMT Pascal}
  _zero = 0;
 
type
  Ponesector = ^onesector;
  oneSector = array[0..511] of byte;

  Tdrvparams = record
    Cylinders:word;
    heads,sectors:byte;
  end;

  TdiskAddressPacket = record {Used in ext BIOS read/write }
    packetsize,  { should be 16d}
    blocks,     {blocks to transfer, up to 127}
    o,s:word; {segment,offset of buffer}
    startlo,starthi:dword; {lo and hi part of LBA *}
  end;

{
****************************************************************************
  (FROM D1226R6 Technical Report )

Starting logical block address, on the target device, of the data to be
transferred. This is a 64 bit unsigned linear address. If the device 
supports LBA addressing this value should be passed unmodified. If the 
device does not support LBA addressing the following formula holds true 
when the address is converted to a CHS value:
	LBA = (C1 * H0 + H1) * S0 + S1 - 1
Where:
	C1 = Selected Cylinder Number
	H0 = Number of Heads (Maximum Head Number + 1)
	H1 = Selected Head Number
	S0 = Maximum Sector Number
  S1 = Selected Sector Number

*****************************************************************************
}

  TpartRec = record  { This matches the table in the MBR}
    active ,
    StartHD:byte;
    StartCylSect:word;
    PartType,
    EndHd:byte;
    EndCylSect:word;
    StartLBA,
    size:dword;
  end;

  Tpartition = object
    DP:Tdrvparams;  { Parameters of current drive }
    part:Tpartrec;
    active:boolean;
    startlba,size:dword;  { LBA written in the partition table, }
    StartPartLBA:dword;   { Actual LBA the partition starts at }
    startcylinder:word;
    starthead,startsector:byte;
    endcylinder:word;
    endsector,endhead:byte;
    partType:String;
    function unknownType:boolean;
    procedure setactive;
    procedure SetPartRec(tablepos:dword;t:byte;StartPos,len:dword);
    procedure setStartLCHS(c:word;h,s:byte);
    procedure setEndLCHS(c:word;h,s:byte);
    procedure setStartLBA(b:dword);
    procedure setSize(b:dword);
    procedure setpartType(t:byte);
    procedure convert(dest:tdrvparams);
    procedure start(d:Tdrvparams;p:Tpartrec);
  end;

  pPartitionTable = ^TpartitionTable;
  TpartitionTable = object
      partition:array[1..4] of Tpartition;
      dp:Tdrvparams;
      Location:dword;
      PartitionSector:OneSector;
      function drivesize:dword;
      function TableEmpty:boolean;
      function PrimaryExists:boolean;
      function TableFull:boolean;
      function ExtendedExists:boolean;
      function DBFSexists:boolean;
      procedure SetEXTpart(start, len:dword);
      function primarypart:word;
      function ExtendedPart:word;
      function DBFSpart:word;
      function StartPrimary:dword;
      function startExtended:dword;
      function startDBFS:dword;
      function largestFreeBlocksize:dword;
      function largestfreeblockstart:dword;
      function PartitionedSpace:dword; {Total partitioned space in this table}
      function NextUnpartitionedSector:dword;
      procedure convert(ndp:tdrvparams);
      procedure UpdatePartitionSector;
      function NextTrack(a:dword):dword;
      constructor init(d:Tdrvparams;ps:onesector;loc:dword);
      destructor finish;
  end;

const
  mbrMaster:onesector =
  (51,192,142,208,188,0,124,251,80,7,80,31,252,190,27,124,191,27,6,80,87,
185,229,1,243,164,203,190,190,7,177,4,56,44,124,9,117,21,131,198,16,226,
245,205,24,139,20,139,238,131,198,16,73,116,22,56,44,116,246,190,16,7,78,
172,60,0,116,250,187,7,0,180,14,205,16,235,242,137,70,37,150,138,70,4,180,
6,60,14,116,17,180,11,60,12,116,5,58,196,117,43,64,198,70,37,6,117,36,187,
170,85,80,180,65,205,19,88,114,22,129,251,85,170,117,16,246,193,1,116,11,
138,224,136,86,36,199,6,161,6,235,30,136,102,4,191,10,0,184,1,2,139,220,
51,201,131,255,5,127,3,139,78,37,3,78,2,205,19,114,41,190,70,7,129,62,254,
125,85,170,116,90,131,239,5,127,218,133,246,117,131,190,39,7,235,138,152,
145,82,153,3,70,8,19,86,10,232,18,0,90,235,213,79,116,228,51,192,205,19,
235,184,0,0,128,72,38,21,86,51,246,86,86,82,80,6,83,81,190,16,0,86,139,
244,80,82,184,0,66,138,86,36,205,19,90,88,141,100,16,114,10,64,117,1,66,
128,199,2,226,247,248,94,195,235,116,73,110,118,97,108,105,100,32,112,97,
114,116,105,116,105,111,110,32,116,97,98,108,101,0,69,114,114,111,114,32,
108,111,97,100,105,110,103,32,111,112,101,114,97,116,105,110,103,32,115,
121,115,116,101,109,0,77,105,115,115,105,110,103,32,111,112,101,114,97,116,
105,110,103,32,115,121,115,116,101,109,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,139,252,30,87,139,245,203,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,245,155,246,145,0,0,128,1,1,0,6,254,63,130,63,0,0,0,132,28,
32,0,0,0,1,131,5,254,127,4,195,28,32,0,2,222,31,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,85,170);


type
  TDA = array[drive_1..drive_4] of boolean;

var
  ErrorStatus:byte;  {should usually match the diskstatus fn}

  {should be 21h for d1226r6 report but 20h should work  ??}
  BIOSextensionS:word; {get this from int 13h, fn 41h (check extensions)}

  Extensions:array[drive_1..drive_4] of word;
  fixed_disk_ext, drive_lock_and_eject_ext, EDD_ext:tda;

  floppy:array[drive_A..drive_B] of Tdrvparams;
  harddisk:array[drive_1..drive_4] of Tdrvparams;


procedure getdrvparams;
function DriveSz(d:byte):dword;
function drivesize(dp:tdrvparams):dword;
procedure driveInfo(drv:byte;var hds:byte;var cyl:word;var sec:byte);
function diskstatus:byte;
procedure resetdisk(drive:byte);
procedure readsectorLBA(drv:byte;b:dword;d:ponesector);
procedure writesectorLBA(drv:byte;b:dword;d:ponesector);
procedure writetrackLBA(drv:byte;b:longint;num:byte;dest:pbuf);
procedure readtracklba(drv:byte;b:longint;num:byte; dest:pbuf);
function error:string;
function DrivesAttached:byte;
function driveExists(drive:byte):boolean;
procedure initDOSmembuffer;
procedure removeDOSmembuffer;



{ Mostly private but still need to be in interface part }
function EmptyPartitionSector:onesector;
function lchstoLBA(c:word;h,s:byte;dp:tdrvparams):dword;
procedure LBAtoLchs(b:dword;var c:word;var h,s:byte;dp:tdrvparams);
function heads(drive:byte):byte;
procedure convertCX(valcx:word;var sect:byte;var cyl:word);
function cylinders(drive:byte):word;
function sectors(drive:byte):byte;
function MakeCX(sect:byte; cyl:word):word;
procedure readTrack(drive,num,sector:byte; track,head:word; dest:pbuf);
procedure readsector(drive:byte;track,head:word;sector:byte; dest:pointer);
procedure writeTrack(drive,num,sector:byte; track,head:word; dest:pbuf);
procedure writesector(drive:byte;track,head:word;sector:byte; dest:pointer);


implementation

var

{ Global pointer for common data area for real mode DOS and BIOS calls }
  dosbufseg:word;
  dosbufSel:word;
  dosbufptr:pointer;

{ For the diskaddress packet of the extended BIOS read/write }
  DiskAddrSeg:word;
  DiskAddrSel:word;
  DiskAddressPtr:pointer;

{ for the block transfer buffer of the extended BIOS}
  DiskTransferseg:word;
  DiskTransferSel:word;
  DiskTransferPtr:pointer;


{*  Creates a common area in DOS memory pool for DOS and BIOS functions
  to use for data structures and disk I/O }
function MkDOSPointer (Segment: Word): Pointer;
begin
  mkdospointer:=pointer(DWord(Segment)*16+_zero);
end;

procedure DosAlloc(var selector : word;	var segment : word; size : longint);
var
	res : longint;
begin
	res := global_dos_alloc(size);
	selector := word(res);
	segment := word(res shr 16);
end;

procedure initDOSmembuffer;
BEGIN
  DosAlloc(dosbufSel, dosbufseg, smallbufsize);
  dosbufptr:=mkDOSpointer(dosbufseg);

  DosAlloc(diskAddrSel, diskAddrSeg, $10); {Size of address packet}
  diskAddressPtr:=mkdospointer(diskAddrSeg);

  DosAlloc(diskTransferSel, diskTransferSeg, (127*512)); {max number of blocks}
  disktransferptr:=mkdospointer(disktransferseg);
END;

(*
procedure initDOSmembuffer;
BEGIN
  dosbufseg:=dosmemoryalloc(smallbufsize);
  diskAddrSeg:=dosmemoryalloc($10);  {Size of address packet}
  dosbufptr:=mkDOSpointer(dosbufseg);
  diskAddressPtr:=mkdospointer(diskAddrSeg);
  diskTransferSeg:=dosmemoryalloc(127*512); {max number of blocks}
  disktransferptr:=mkdospointer(disktransferseg);
END;
*)

procedure removeDOSmembuffer;
begin
 	global_dos_free(dosbufsel);
 	global_dos_free(diskaddrSel);
 	global_dos_free(disktransferSel);
end;


  procedure DAPread(b:dword; len:byte);
  var dap:TdiskaddressPacket;
  begin
    with DAP do
    begin
      packetsize:=$10;
      blocks:=len;
      s:=DiskTransferSeg;
      o:=0;
      startlo:=b;
      starthi:=0;
    end;
    move(DAP,diskaddressptr^,$10);
  end;

  procedure DAPwrite(b:dword; len:byte; var buf);
  var DAP:TdiskAddressPacket;
  begin
    move(buf, diskTransferPtr^, 512*len);
    with DAP do
    begin
      packetsize:=$10;
      blocks:=len;
      s:=DiskTransferSeg;
      o:=0;
      startlo:=b;
      starthi:=0;
    end;
    move(DAP,diskAddressPtr^,$10);
  end;


{ 40:75 is in the table BIOS builds on post to show what hardware has been
  found.  Here it puts the number of fixed disks.  You can then find if
  a disk exists easily because there are no gaps in the numbering of BIOS
  drives.  They start at 80h so if you want to know if drive 83h exists,
  just add 80h and drivesattached if it is lower than 83h the drive does not
  exist. }

function DrivesAttached:byte;
var d:byte absolute $40:$75;
begin
    drivesattached:=d;
end;

function driveExists(drive:byte):boolean;
begin
  if drive>127 then drive:=drive-127;
  if drive>drivesattached then driveExists:=false else driveExists:=true;
end;

function EmptyPartitionSector:onesector;
var e:onesector;
    i:word;
begin
  e:=mbrMaster;
  for i:=part1 to part4+15 do e[i]:=0;
  EmptyPartitionSector:=e;
end;


function drivesz(d:byte):dword;
var c1,s1,h1:dword;
    dp:tdrvparams;
begin
  if d in harddisks then dp:=harddisk[d];
  if d in floppies then dp:=floppy[d];

  s1:=dp.sectors;
  h1:=dp.heads;
  c1:=dp.cylinders;
  if (c1=0) or (s1<2) or (h1<1) then drivesz:=0 else
  drivesz:=(s1*(h1+1)*(c1+1)) div 2048;
end;


function drivesize(dp:tdrvparams):dword;
var s1,h1:dword;
    c1:dword;
begin
  s1:=dp.sectors;
  h1:=dp.heads;
  c1:=dp.cylinders;
  if (c1=0) or (s1<2) or (h1<1) then drivesize:=0 else
  drivesize:=(s1*(h1+1)*(c1+1));
end;

function disknum(drive:byte):byte;
var regs:TRealRegs;
begin
    fillchar(regs, sizeof(regs), 0);
    with regs do
    begin
      ah:=08;
      dl:=drive;
      Realintr($13,regs);
      disknum:=dl;
  end;
end;

function lchstoLBA(c:word;h,s:byte;dp:tdrvparams):dword;
var s5,c5,h5,cl,hl,sl:dword;
begin
  cl:=c;
  hl:=h;
  sl:=s;

  s5:=dp.sectors;
  c5:=dp.cylinders+1;
  h5:=dp.heads+1;

  lchstolba:=((cl * h5+ hl)*s5) + sl - 1;
end;

procedure LBAtoLchs(b:dword;var c:word;var h,s:byte;dp:tdrvparams);
var Newsec,Newhead:byte;
    Newcyl:word;
    tmp:dword;
begin
  with dp do
  begin
       newcyl := b div ((heads+1) * sectors);
        tmp := b mod ((heads+1) * sectors);
      newhead := tmp div sectors;
       newsec := tmp  mod  sectors + 1;
  end;

  c:=newcyl;
  h:=newhead;
  s:=newsec;
end;


function heads(drive:byte):byte;
var regs:TRealRegs;
begin
  fillchar(regs, sizeof(regs), 0);
  with regs do
  begin
    ah:=$08;
    dl:=drive;
    Realintr($13,regs);
    heads:=dh;
  end;
end;


procedure convertCX(valcx:word;var sect:byte;var cyl:word);
var c,sec:byte;
    x:word;
    regs:TRealRegs;
begin
  fillchar(regs, sizeof(regs), 0);
  with regs do
  begin
      cx:=valcx;
      bx:=valcx;

      c:=ch;
      cx:=cx and 192;

{      and cx,0000000011000000b}

      cx:=cx shl 2;
      x:=cx;
      cx:=bx;
      cl:=cl and 63;
{      and cl,00111111b}
    sect:=cl;
    cyl:=x+c;
   end;
end;

function cylinders(drive:byte):word;
var regs:TRealRegs;
begin
  fillchar(regs, sizeof(regs), 0);
  with regs do
  begin
    ah:=$08;
    dl:=drive;
    Realintr($13,regs);
    dx:=0;
    dl:=ch;
    cx:=cx and 192;
{      and cx,0000000011000000b}
    cx:=cx shl 2;
    cx:=cx + dx;
    cylinders:=cx;
  end;
end;

function sectors(drive:byte):byte;
var regs:TRealRegs;
begin
  fillchar(regs, sizeof(regs), 0);
  with regs do
  begin
    ah:=$08;
    dl:=drive;
    Realintr($13,regs);
    cl:=cl and 63;
    sectors:=cl;
  end;
end;


procedure driveInfo(drv:byte;var hds:byte;var cyl:word;var sec:byte);
begin
  hds:=0;
  cyl:=0;
  sec:=0;

  resetdisk(drv);
  if diskstatus<>0 then exit;
  hds:=heads(drv);
  if diskstatus<>0 then exit;
  cyl:=cylinders(drv);
  if diskstatus<>0 then exit;
  sec:=sectors(drv);
  if diskstatus<>0 then
  begin
    sec:=0;
    hds:=0;
    cyl:=0;
  end;
end;


procedure getdrvparams;
var I, dn:byte;
    regs:TRealRegs;
begin
  resetdisk($80);

  for i:=drive_A to drive_B do
    with floppy[i] do
      driveinfo(i,heads,cylinders,sectors);

  for i:=drive_1 to drive_4 do
    with harddisk[i] do
      if driveExists(i) then
        driveinfo(i,heads,cylinders,sectors)
       else
        begin
          heads:=0;
          cylinders:=0;
          sectors:=0;
        end;

  BIOSextensions:=0;
  for i:=drive_1 to drive_4 do
  begin
    fixed_disk_ext[i]:=false;
    drive_lock_and_eject_ext[i]:=false;
    EDD_ext[i]:=false;
  end;

  for i:=drive_1 to drive_4 do
  begin
    fillchar(regs, sizeof(regs), 0);
    with regs do
    begin
      ah:=$41; {check extensions fn }
      bx:=$55aa;  {sys signature word }
      dl:=i;  { drive 1 to drive 4}
      Realintr($13, regs);
      if (not odd(flags)) and (bx=$aa55) then
      begin
        BIOSextensions:=ah;
        if odd(cx) then fixed_disk_ext[i]:=true;
        if odd(cx shr 1) then drive_lock_and_eject_ext[i]:=true;
        if odd(cx shr 2) then EDD_ext[i]:=true;
        extensions[i]:=cx;
      end else extensions[i]:=0;
    end;
  end;
end;

{ From D1226r6 Technical Report:

  Check extensions present

	Entry:
		AH - 41h
		BX - 55AAh
		DL - Drive number
	Exit:
		carry clear
			AH - Version of extensions
			AL - Internal use only
			BX - AA55h
			CX - Interface support bit map (see Table 9Table 12 10)
		carry set
			AH - error code (01h, Invalid Command)

Table 12109 ? Extension rResult buffer
Bit
Description
0
1 - Fixed disk access subset
1
1 - Drive locking and ejecting subset
2
1 - Enhanced disk drive support subset
3-15
Reserved, must be 0
}


function diskstatus:byte;
var regs:TRealRegs;
begin
  fillchar(regs, sizeof(regs), 0);
  with regs do

  begin
    ah:=1;
    Realintr($13,regs);
    diskstatus:=al;
  end;
end;

procedure resetdisk(drive:byte);
var regs:TRealRegs;
begin
  with regs do
  begin
    ah:=ah xor ah;
    dl:=drive;
    Realintr($13,regs);
  end;
end;

function MakeCX(sect:byte; cyl:word):word;
var regs:TRealRegs;
begin
  with regs do
  begin
    bx:=cyl;
    ax:=cyl;
    ax:=ax shr 2;
    ax:=ax and 192;
    al:=al + sect;
    bx:=bx shl 8;
    bx:=bx and 65280;
{    and bx,1111111100000000b}
    bx:=bx+ax;
    makeCX:=bx;
  end;
end;


procedure readTrack(drive,num,sector:byte;track,head:word; dest:pbuf);
var
    regs : TRealRegs;
    tcx:word;
begin
    fillchar(regs, sizeof(regs), 0);
    tcx:= makeCX(sector,track);
    with regs do
    begin
      ah:=2;
      al:=num;
      dl:=drive;
      cx:=TCX;
      bx:=head;
      dh:=bl;
      bx:=0;
      es:=dosbufseg;
      Realintr($13,regs);
    end;
    move(dosbufptr^,dest^,num*512);
end;


{ read one sector }
procedure readsector(drive:byte;track,head:word;sector:byte; dest:pointer);
var
    regs : TRealRegs;
    tcx:word;
begin
    fillchar(regs, sizeof(regs), 0);
    tcx:= makeCX(sector,track);
    with regs do
    begin
      ah:=2;
      al:=1;
      dl:=drive;
      cx:=TCX;
      bx:=head;
      dh:=bl;
      bx:=0;
      es:=dosbufseg;
      Realintr($13,regs);
    end;
    move(dosbufptr^,dest^,512);
end;

procedure EXTread(drv:byte; b:dword; d:ponesector);
var regs:TRealRegs;
begin
  DAPread(b, 1);  {prepare disk address packet}
  with regs do
  begin
    fillchar(regs, sizeof(regs), 0);
    ah:=$42;  {extended read fn}
    al:=0;
    dl:=drv;
    ds:=diskAddrSeg;
    si:=0;
    Realintr($13, regs);
    if odd(flags) then
    begin
      if diskstatus<>0 then ErrorStatus:=diskstatus
        else Errorstatus:=BadSector;
      exit;
    end;
  end;
  move(diskTransferPtr^, d^, 512);
end;


procedure EXTreadTrack(drv:byte; b:dword; num:byte; d:pbuf);
var regs:TRealRegs;
begin
  DAPread(b, num);  {prepare disk address packet}
  with regs do
  begin
    fillchar(regs, sizeof(regs), 0);
    ah:=$42;  {extended read fn}
    al:=0;
    dl:=drv;
    ds:=diskAddrSeg;
    si:=0;
    Realintr($13, regs);
    if odd(flags) then
    begin
      if diskstatus<>0 then ErrorStatus:=diskstatus
        else Errorstatus:=BadSector;
      exit;
    end;
  end;
  move(diskTransferPtr^, d^, 512*num);
end;

procedure extWrite(drv:byte; b:dword; d:ponesector);
var regs:TRealRegs;
begin
  DAPwrite(b, 1, d^);
  fillchar(regs, sizeof(regs), 0);

  with regs do
  begin
    ah:=$43; { ext write fn}
    al:=1;  { 1 =  write with verify off -- faster }
    dl:=drv;
    ds:=diskAddrSeg;
    si:=0;

    Realintr($13, regs);

    if odd(flags) then
    begin
      if diskstatus<>0 then ErrorStatus:=diskstatus
        else Errorstatus:=BadSector;
      exit;
    end;
  end;
end;


procedure extWriteTrack(drv:byte; b:dword; num:byte; d:pbuf);
var regs:TRealRegs;
begin
  DAPwrite(b, num, d^);
  fillchar(regs, sizeof(regs), 0);
  with regs do
  begin
    ah:=$43; { ext write fn}
    al:=1;  { 1 =  write with verify off -- faster }
    dl:=drv;
    ds:=diskAddrSeg;
    si:=0;
    Realintr($13, regs);
    if odd(flags) then
    begin
      if diskstatus<>0 then ErrorStatus:=diskstatus
        else Errorstatus:=BadSector;
      exit;
    end;
  end;
end;


procedure readsectorLBA(drv:byte;b:dword;d:ponesector);
var c:word;
    s,h:byte;
    CurDrvParams:tdrvparams;
begin
  if (drv in harddisks) and fixed_disk_ext[drv] then
    ExtRead(drv, b, d)
  else begin
    if drv in floppies then curdrvparams:=floppy[drv]
      else curdrvparams:=hardDisk[drv];
    lbatolchs(b,c,h,s,curdrvparams);
    readsector(drv,c,h,s,d);
  end;
end;


procedure readtracklba(drv:byte;b:longint;num:byte;dest:pbuf);
var dp:Tdrvparams;
    c,p,tcx:word;
    n1,n2,h,s:byte;
    tmpptr:pointer;
    h1:word;
begin
  if (drv in harddisks) and fixed_disk_ext[drv] then
    ExtReadTrack(drv, b, num, dest)
  else
  begin
    if drv in floppies then dp:=floppy[drv] else dp:=hardDisk[drv];
    lbatolchs(b,c,h,s,dp);

    n2:=0;
    n1:=dp.sectors-(s-1);
    if num<n1 then n1:=num;
    if num>n1 then n2:=num-n1;

    h1:=h;
    readtrack(drv,n1,s,c,h1,dest);
    if n2>0 then
    begin
      b:=b+n1;
      lbatolchs(b,c,h,s,dp);
      h1:=h;

      getmem(tmpptr,n2*512);
      readtrack(drv,n2,s,c,h1,tmpptr);
      move(tmpptr^,dest^[n1*512],n2*512);
      freemem(tmpptr,n2*512);
    end;
  end;
end;


procedure writeTrack(drive,num,sector:byte; track,head:word; dest:pbuf);
var
    regs : TRealRegs;
    tcx:word;
begin
    fillchar(regs, sizeof(regs), 0);
    tcx:= makeCX(sector,track);
    with regs do
    begin
      ah:=3;
      al:=num;
      dl:=drive;
      cx:=TCX;
      bx:=head;
      dh:=bl;
      bx:=0;
      es:=dosbufseg;
      move(dest^,dosbufptr^,num*512);
      Realintr($13,regs);
    end;
end;


procedure writetrackLBA(drv:byte;b:longint;num:byte;dest:pbuf);
var dp:Tdrvparams;
    tmpPtr:pointer;
    c,p,tcx:word;
    n1,n2,h,s:byte;
    h1:word;
begin
 if (drv in harddisks) and fixed_disk_ext[drv] then
    ExtwriteTrack(drv, b, num, dest)
 else
 begin
    if drv in floppies then dp:=floppy[drv] else dp:=hardDisk[drv];
    lbatolchs(b,c,h,s,dp);
    h1:=h;

    n2:=0;
    n1:=dp.sectors-(s-1);
    if num>n1 then n2:=num-n1;
    if num<n1 then n1:=num;

    writetrack(drv,n1,s,c,h1,dest);

    if n2>0 then
    begin
      b:=b+n1;
      lbatolchs(b,c,h,s,dp);
      h1:=h;

      getmem(tmpptr,n2*512);
      move(dest^[n1*512],tmpptr^,n2*512);
      writetrack(drv,n2,s,c,h1,tmpptr);
      freemem(tmpptr,n2*512);
    end;
  end;
end;

procedure writesector(drive:byte;track,head:word;sector:byte; dest:pointer);
var regs : TRealRegs;
    tcx:word;
begin
    fillchar(regs, sizeof(regs), 0);
    tcx:= makeCX(sector,track);
    with regs do
    begin
      ah:=3;
      al:=1;
      dl:=drive;
      cx:=TCX;
      bx:=head;
      dh:=bl;
      bx:=0;
      es:=dosbufseg;
      move(dest^,dosbufptr^,512);
      Realintr($13,regs);
    end;
end;

procedure writesectorLBA(drv:byte;b:dword;d:ponesector);
var c:word;
    s,h:byte;
    CurDrvParams:tdrvparams;
begin
  if (drv in harddisks) and fixed_disk_ext[drv] then
    Extwrite(drv, b, d)
  else
  begin
    if drv in floppies then curdrvparams:=floppy[drv] else
    curdrvparams:=harddisk[drv];

   lbatolchs(b,c,h,s,curdrvparams);
    writesector(drv,c,h,s,d);
  end;
end;

{
procedure readtracklba(drv:byte;b:dword;num:byte;var dest);
var dp:Tdrvparams;
    c,p,tcx:word;
    n1,n2,h,s:byte;
    tmp1,tmp2:array[1..40000] of byte;
    h1:word;
begin


  if drv in floppies then dp:=floppy[drv] else dp:=hardDisk[drv];
  lbatolchs(b,c,h,s,dp);

  n2:=0;
  n1:=dp.sectors-(s-1);
  if num<n1 then n1:=num;
  if num>n1 then n2:=num-n1;

  h1:=h;
  readtrack(drv,n1,s,c,h1,tmp1);
  if n2>0 then
  begin
    b:=b+n1;
    lbatolchs(b,c,h,s,dp);
    h1:=h;

    readtrack(drv,n2,s,c,h1, tmp2);

    move(tmp2,tmp1[n1*512],n2*512);
    move(tmp1,dest,(n1+n2)*512);
  end
  else move(tmp1,dest,n1*512);


end;


procedure writetrackLBA(drv:byte;b:dword;num:byte;var dest);
var dp:Tdrvparams;
    c,p,tcx:word;
    tmp1:array[1..40000] of byte;
    n1,n2,h,s:byte;
    h1:word;
begin
  if drv in floppies then dp:=floppy[drv] else dp:=hardDisk[drv];
  lbatolchs(b,c,h,s,dp);
  h1:=h;

  n2:=0;
  n1:=dp.sectors-(s-1);

  if num>n1 then n2:=num-n1;
  if num<n1 then n1:=num;

  move(dest, tmp1, num*512);

  writetrack(drv,n1,s,c,h1,tmp1);

  if n2>0 then
  begin
    b:=b+n1;
    lbatolchs(b,c,h,s,dp);
    h1:=h;

    move(tmp1[n1*512],tmp1, n2*512);

    writetrack(drv,n2,s,c, h1, tmp1);
  end;
end;
}

function error:string;
var errors:string;
    d:byte;
begin
  errors:='None';
  d:=diskstatus;
  case d of
    noerror:errors:='None';
    badcommand:errors:='Bad command';
    badsector:errors:='Bad sector';
    writeprotected:errors:='Write Protected';
    sectornotfound :errors:='Sector not found';
    diskchange :errors:='Disk changed';
    invalidmedia :errors:='Invalid media';
    controllererror:errors:='Controller error';
    seekfailure :errors:='Seek failure';
    timeout :errors:='Time out error';
    NoDPB:errors:='Failed to acquire DPB';
    NOTFAT16:errors:='Non-FAT file system';
{    else errors:=int2hexstr(d);}
  end;
  error:=errors;
end;

procedure writefile(var f:file;var d;sz:word;var nr:word);
begin
  blockwrite(f,d,sz,nr);
end;


{**************************************************************************}
{   Partition record methods                                                }
{***************************************************************************}

procedure Tpartition.start(d:Tdrvparams;p:Tpartrec);
var s:string;
    sw:byte;
    c:word;
begin
  dp:=d;
  part:=p;
  if part.active=80 then active:=true else active:=false;
  startlba:=part.startlba;
  size:=part.size;
  case part.parttype of
      0:s:='Unknown';
      1:s:='FAT12';
      2:s:='XENIX';
      4:s:='DOS FAT16';
      15:s:='Extended DOS (LBA)';
      5:s:='Extended DOS';
      6:s:='FAT16';
      7:s:='NTFS';
      11:s:='FAT32';
      12:s:='FAT32 (LBA)';
      14:s:='FAT16 (LBA)';
      51:s:='Ontrack extended partition';
      64:s:='Novell';
      75:s:='PCIX';
      160:s:='Phoenix Save To Disk';
      $db:s:='CP/M';
      $E0:s:='DBFS';
      $FF:s:='BBT';
      else str(part.parttype,s);
    end;
    partType:=s;

     convertcx(part.startcylsect,sw,c);
     startsector:=sw;
     startcylinder:=c;
     starthead:=part.starthd;
     convertcx(part.endcylsect,sw,c);
     endsector:=sw;
     endcylinder:=c;
     endhead:=part.endhd;
     startpartlba:=lchstolba(startcylinder,starthead,startsector,DP);
end;


function Tpartition.unknownType:boolean;
begin
  case part.partType of
    4,5,6,14,15,NTFS,DBFS,FAT32,FAT32x:unknownType:=false;
    else unknownType:=true;
  end;
end;

procedure Tpartition.SetStartLCHS(c:word;h,s:byte);
begin
  part.starthd:=h;
  part.startCylsect:=makeCX(s,c);
  start(dp,part);
end;

procedure Tpartition.SetEndLCHS(c:word;h,s:byte);
begin
  part.endhd:=h;
  part.endcylsect:=makecx(s,c);
  start(dp,part);
end;

procedure Tpartition.SetStartLBA(b:dword);
begin
  part.startlba:=b;
  start(dp,part);
end;

procedure Tpartition.SetSize(b:dword);
begin
  part.size:=b;
  start(dp,part);
end;

procedure Tpartition.setPartType(t:byte);
begin
  part.partType:=t;
  start(dp,part);
end;

procedure Tpartition.SetActive;
begin
  part.active:=$80;
  start(dp,part);
end;

procedure Tpartition.SetPartRec(tablepos:dword;t:byte;StartPos,len:dword);
var c:word;
    h,s:byte;
begin
  if (startpos<drivesize(dp)) and ((startpos+len)<=drivesize(dp)) then
  begin
    lbatolchs(startpos,c,h,s,DP);
    setstartLCHS(c,h,s);
    lbatolchs(startpos+len,c,h,s,DP);
    setEndLCHS(c,h,s);
    SetpartType(t);
    setSize(len);
    SetStartLBA(startpos-tablepos);
  end;
end;


{ Converts the LCHS params which are assumed to be for a drive with DP params }
{ and converts the addresses to a drive with DEST params }
procedure Tpartition.Convert(dest:Tdrvparams);
begin
  lbatolchs(lchstolba(StartCylinder,StartHead,StartSector,dp),
          StartCylinder,StartHead,StartSector,Dest);

  LBAtoLCHS(LCHStoLBA(EndCylinder,EndHead,EndSector,dp),
            EndCylinder,EndHead,EndSector,Dest);

  dp:=dest;
  SetStartLCHS(StartCylinder,StartHead,Startsector);
  SetEndLCHS(Endcylinder,EndHead,EndSector);
end;


{ *************************************************************************** }
{ Partition Table methods                                                   }
{ *************************************************************************}

procedure Tpartitiontable.convert(ndp:tdrvparams);
var i:word;
begin
  for i:=1 to 4 do
    partition[i].convert(ndp);
  dp:=ndp;
  move(partition[1].part,partitionsector[part1],16);
  move(partition[2].part,partitionsector[part2],16);
  move(partition[3].part,partitionsector[part3],16);
  move(partition[4].part,partitionsector[part4],16);
  partition[1].start(ndp,partition[1].part);
  partition[2].start(ndp,partition[2].part);
  partition[3].start(ndp,partition[3].part);
  partition[4].start(ndp,partition[4].part);
end;

procedure TpartitionTable.updatePartitionSector;
begin
  move(partition[1].part,partitionsector[part1],16);
  move(partition[2].part,partitionsector[part2],16);
  move(partition[3].part,partitionsector[part3],16);
  move(partition[4].part,partitionsector[part4],16);
end;


constructor TpartitionTable.init(d:Tdrvparams;ps:onesector;loc:dword);
var curdrv:TdrvParams;
    p:Tpartrec;
    i:word;
begin
  location:=loc;
  partitionsector:=ps;
  curdrv:=d;
  dp:=d;
  move(ps[part1],p,16);
  partition[1].start(curdrv,p);
  move(ps[part2],p,16);
  partition[2].start(curdrv,p);
  move(ps[part3],p,16);
  partition[3].start(curdrv,p);
  move(ps[part4],p,16);
  partition[4].start(curdrv,p);
  for i:=1 to 4 do
    if partition[i].size=0 then partition[i].startpartlba:=0;
end;

function Tpartitiontable.drivesize:dword;
var s1,c1,h1:dword;
begin
  s1:=dp.sectors;
  h1:=dp.heads;
  c1:=dp.cylinders;
  drivesize:=s1*(h1+1)*(c1+1);
end;

function TpartitionTable.primaryExists:boolean;
begin
  IF STARTPRIMARY=0 THEN PRIMARYEXISTS:=false ELSE PRIMARYEXISTS:=true;
end;

function TpartitionTable.PrimaryPart:word;
var i,p:word;
begin
  p:=0;
  for i:=1 to 4 do if partition[i].part.partType in FATtypes then p:=i;
  primarypart:=p;
end;

function TpartitionTable.TableEmpty:boolean;
var i:word;
    e:boolean;
begin
  e:=true;
  for i:=1 to 4 do if partition[i].size>0 then e:=false;
  TableEmpty:=e;
end;


function TpartitionTable.ExtendedPart:word;
var i,p:word;
begin
  p:=0;
  for i:=1 to 4 do if (partition[i].part.partType in EXTtypes) then p:=i;
  ExtendedPart:=p;
end;


function TpartitionTable.DBFSPart:word;
var i,p:word;
begin
  p:=0;
  for i:=1 to 4 do if (partition[i].part.partType = DBFS) then p:=i;
  DBFSPart:=p;
end;



function Tpartitiontable.extendedexists:boolean;
BEGIN
  IF STARTEXTENDED=0 THEN EXTENDEDEXISTS:=false ELSE extendedexists:=true;

end;

function Tpartitiontable.DBFSexists:boolean;
BEGIN
  IF STARTDBFS=0 then DBFSEXISTS:=false ELSE DBFSexists:=true;
end;

function Tpartitiontable.TableFull:boolean;
var f:boolean;
    i:word;
begin
  f:=true;
  for i:=1 to 4 do if partition[i].part.parttype=0 then f:=false;
  TableFull:=f;
end;

procedure TpartitionTable.SetEXTpart(start, len:dword);
var i:word;
    partitioned:boolean;
begin
  partitioned:=false;
  if not ExtendedExists then
  begin
    if tablefull then exit;
    for i:=1 to 4 do if (partition[i].part.parttype=0) and
      (not partitioned)  then
    begin
      partition[i].setpartrec(0, EXT, start, len);
      partitioned:=true;
    end;
  end else
    partition[Extendedpart].setPartrec(0,
            partition[ExtendedPart].part.parttype,
             start, len);
end;


{ This works directly on variable data not pointers to the data, so if you have }
{ large amounts of data it would be better to just swap the pointers to the data}
{ Ex:  swap(ptr1,ptr2,sizeof(ptr1)); is faster than swap(ptr1^,ptr2^,1000); }
{ in the case where ptr1 and ptr2 point to structures 1000 bytes large. }
procedure swap(var a,b;sz:word);
var p:pointer;
begin
  getmem(p,sz);
  move(b,p^,sz);
  move(a,b,sz);
  move(p^,a,sz);
  freemem(p,sz);
end;


function TpartitionTable.NextTrack(a:dword):dword;
begin
  nextTrack:=a + dp.sectors - (a mod dp.sectors)
end;

function TpartitionTable.PartitionedSpace:dword;
var i:byte;
    s:dword;
begin
  s:=0;
  for i:=1 to 4 do s:=s+partition[i].size;
  partitionedSpace:=s;
end;

function Tpartitiontable.nextUnpartitionedsector:dword;
var i:byte;
    endPos:dword;
begin
  endpos:=0;
  for i:=1 to 4 do with partition[i] do
    if (startpartlba+size) > endpos then endpos:=(startpartlba+size);
  NextUnPartitionedSector:=nextTrack(endPos);
end;

{ Sorts the main partition table, then finds the largest unpartition space }
{ on the disk. }
function Tpartitiontable.LargestFreeBlocksize:dword;
type tps = record start,size:dword; end;
VAR B:ARRAY[1..4] OF tps;
    free:array[1..5] of tps;
    i,j,largest:word;
begin
  LARGEST:=1;

  for i:=1 to 4 do
  begin
    b[i].start:=partition[i].startpartlba;
    b[i].size:=partition[i].size;
  end;

  for i:=1 to 3 do for j:=i+1 to 4 do
    if b[i].start>b[j].start then swap(b[i],b[j],sizeof(b[i]));

  free[1].size:=b[1].start;
  free[1].start:=0;

  free[2].size:=b[2].start-(b[1].size+b[1].start);
  free[2].start:=b[1].start+b[1].size;

  free[3].size:=b[3].start-(b[2].start+b[2].size);
  free[3].start:=b[2].start+b[2].size;

  free[4].size:=b[4].start-(b[3].start+b[3].size);
  free[4].start:=b[3].start+b[3].size;

  free[5].size:=drivesize-(b[4].start+b[4].size);
  free[5].start:=b[4].start+b[4].size;

    for i:=1 to 4 do if free[i].size<free[i+1].size then largest:=i+1;

    largestfreeblocksize:=free[largest].size;
end;


function Tpartitiontable.LargestFreeBlockstart:dword;
type tps = record start,size:dword; end;
VAR B:ARRAY[1..4] OF tps;
    free:array[1..5] of tps;
    i,j,largest:word;
begin
  LARGEST:=1;

  for i:=1 to 4 do
  begin
    b[i].start:=partition[i].startpartlba;
    b[i].size:=partition[i].size;
  end;

  for i:=1 to 3 do for j:=i+1 to 4 do
    if b[i].start>b[j].start then swap(b[i],b[j],sizeof(b[i]));

  free[1].size:=b[1].start;
  free[1].start:=0;

  free[2].size:=b[2].start-(b[1].size+b[1].start);
  free[2].start:=b[1].start+b[1].size;

  free[3].size:=b[3].start-(b[2].start+b[2].size);
  free[3].start:=b[2].start+b[2].size;

  free[4].size:=b[4].start-(b[3].start+b[3].size);
  free[4].start:=b[3].start+b[3].size;

  free[5].size:=drivesize-(b[4].start+b[4].size);
  free[5].start:=b[4].start+b[4].size;

  for i:=1 to 4 do if free[i].size<free[i+1].size then largest:=i+1;

  largestfreeblockstart:=free[largest].start;
end;

{returns the absolute LBA where the FAT partition starts }
function tpartitionTable.startprimary:dword;
var i:word;
begin
  i:=0;
  repeat inc(i) until (partition[i].part.partType in FATtypes) or (i>3);
  if partition[i].part.parttype in FATTypes then
    startprimary:=partition[i].StartPartLBA else startprimary:=0;
end;


function tpartitionTable.startDBFS:dword;
var i:word;
begin
  i:=0;
  repeat inc(i) until (partition[i].part.partType =DBFS) or (i>3);
  if partition[i].part.parttype = DBFS then
    startDBFS:=partition[i].StartPartLBA else startDBFS:=0;
end;

function tpartitiontable.startextended:dword;
var i:word;
begin
  i:=0;
  repeat inc(i) until (partition[i].part.parttype in EXTtypes) or (i>3);
  if (partition[i].part.parttype in EXTtypes) then
    startEXTENDED:=partition[i].StartPartLBA else startEXTENDED:=0;
end;

destructor TpartitionTable.finish;
begin
end;

procedure readfile(var f:file;var d;sz:word;var nr:word);
begin
  blockread(f,d,sz,nr);
end;


end.
