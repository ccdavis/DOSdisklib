program Test;

uses pdisk322,crt;

procedure DispDriveSize(Sects : Comp);
Var Megs, Gigs : Comp;
Begin
 Megs := ((((Sects*512) / 1024)) / 1024);
 Gigs := (((((Sects*512) / 1024)) / 1024) / 1024);

 writeln('Megabytes:', Round(Megs));
 writeln('Gigabytes:', Round(Gigs));
end;

var infobuf : FDG_STRUCT;
begin
  getdrvparams;

  GetEnhDriveSize($80, infobuf);

  DispDriveSize(infobuf.sectorsondrivelo);
end.
