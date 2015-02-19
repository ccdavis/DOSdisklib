unit dosmem;

interface

uses go32;

const _zero = 0;
	DOSvideo = $b800;
	DOSvga = $0a00;

type Tdosmem = object
     buf:array[0..64000] of byte;
     size:word;
     offs,segment, sel:word;
     usedSelector:boolean;
     lasterrorstate:word;
     procedure get;
     procedure get(var b);
     procedure get(var b;sz:word);
     procedure put;
     procedure put(var b);
     procedure put(var b;sz:word);
     procedure geterrorstate;
     constructor init(sz:longint);
     constructor init(sz:longint; seg:word);
     destructor done;
   end;

implementation

procedure Tdosmem.geterrorstate;
begin
     lasterror:= int31error;
     
 end;
                 

// Get DOS memory from segment, of size Size, putting into  Buf
procedure Tdosmem.get;
begin
	DOSmemget(segment,offs,buf,size);
end;

// Get mem from DOS memory, of size Size, and put it in b
procedure Tdosmem.get(var b);
begin
	DOSmemget(segment,offs,b,size);
end;

procedure Tdosmem.get(var b;sz:word);
begin
	DOSmemget(segment,offs,b,sz);
end;

procedure Tdosmem.put;
begin
	DOSmemput(segment,offs,buf,size);
end;

procedure Tdosmem.put(var b);
begin
	DOSmemput(segment,offs,b,size);
end;

procedure Tdosmem.put(var b; sz:word);
begin
	DOSmemput(segment,offs,b,sz);
end;



// Find a new free segment and allocate DOS memory
constructor Tdosmem.init(sz:longint);
var res:longint;
begin
offs:=0;
	res := global_dos_alloc(sz);
	sel := word(res);
	segment := word(res shr 16);
	size:=sz;
	usedSelector:=true;
end;

// We already have a segment in mind, seg points to it
constructor Tdosmem.init(sz:longint; seg:word);
begin
offs:=0;
	usedSelector:=false;
	size:=sz;	
	segment:=seg;
end;

destructor Tdosmem.done;
begin
	if usedSelector then global_dos_free(sel);
end;

end.

