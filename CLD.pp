Program CLD;

uses pdisk32,crt;

var emptysector:ponesector;

procedure clear(drv:byte);
var i,total:longint;
    dp:tdrvparams;
    percent,oldpercent:word;

begin
  writeln;
  write('Please wait'+#13);
  if drv in floppies then dp:=floppy[drv] else dp:=harddisk[drv];
  total:=drivesize(dp)-1;
  oldpercent:=0;
  percent:=0;

  for i:=0 to total do
  begin
    writesectorLBA(drv,i,emptysector);
    percent:=round((i / total)*100);
    if percent>oldpercent then
    begin
      write(#13+'Erasing drive  ',percent,'%     '+#13);
      oldpercent:=percent;
    end;
  end;
end;

procedure main;
var i:word;
    drv:byte;
begin
  new(emptysector);
  for i:=0 to 511 do emptysector^[i]:=0;
  getdrvparams;
  writeln;
  writeln('Disk Eraser by Colin C. Davis');
  writeln;
  writeln('WARNING:  This program will erase a disk.   You cannot recover the data');
  writeln('on the erased disk once you have run the program. Be careful with it.');
  writeln;
  write('Fixed disks:  ');
  for i:=drive_1 to drive_4 do
      if driveExists(i) then write(i-127,' ',drivesz(i),' mb  ');
  writeln;
  if drivesize(floppy[drive_A]) > 0 then
  begin
    write('Floppy ');
    write('A: ',drivesize(floppy[drive_a]) div 2,' kb ');
    if drivesize(floppy[drive_b]) > 0 then
      write('B: ',drivesize(floppy[drive_b]),' kb ');
    writeln;
  end;

  write('Clear drive (1-4,A,B), any other key to abort: ');
  repeat until keypressed;
  case readkey of
    '1':drv:=drive_1;
    '2':drv:=drive_2;
    '3':drv:=drive_3;
    '4':drv:=drive_4;
    'a','A':drv:=drive_a;
    'B','b':drv:=drive_b;
    else halt;
  end;

  writeln;
  write('Are you sure ?  (y/n):');
  repeat until keypressed;
  if upcase(readkey) <>'Y' then halt;

  clear(drv);
  writeln;
  writeln('Done');
  dispose(emptysector);

end;

begin
  InitDOSmemBuffer;
  main;
  RemoveDOSmemBuffer;
end.
