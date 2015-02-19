program test;

uses  dos,dosmem,go32;
{$ASMMODE INTEL}

function EXTsize(drv:byte):dword;

Type
   FDG_STRUCT = record
   bufsize,           {size of buffer}
   flags : word;      {information flags}
   cylinders,
   heads,
   sectorsPerTrack : dword;
   sectorsOnDrivelo,
   sectorsondrivehi: Dword;
   bytesPerSector : word;
end;

var
       r : trealregs;
         infobuf : FDG_STRUCT;
         dosbuf:Tdosmem;

begin
	dosbuf.init(30);
         fillchar(r, sizeof(r), 0);
         
         fillchar(infobuf, sizeof(FDG_STRUCT), 0);

	infobuf.flags := 0;
	infobuf.bufsize:=30;
	dosbuf.put(infobuf);

	With r do
   	Begin
     		flags := 0;
	     	ah:=$48;
     		dl:=$80;
	     	ds :=dosbuf.segment;
     	si := 0;
   	end;
         realintr($13, r);                  

	dosbuf.get(infobuf);
	dosbuf.done;
	EXTsize:=infobuf.sectorsondrivelo;
	dosbuf.done;
   end;

