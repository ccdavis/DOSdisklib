
unit SMALLBUF;

interface
const
  smallbufsize = 40000;

type
  pBuf = ^buf;
  buf = array[0..smallbufsize] of byte;

  Tbuffer = object
      size,start:word;
      storage:pBuf;
      constructor init;
      destructor done;
      procedure get(var d;amount:word;var amountgot:word);
      procedure put(var d;amount:word;var amountput:word);
      procedure restart;
  end;

implementation

constructor Tbuffer.init;
begin
  new(storage);
  size:=0;
  start:=0;
end;

destructor Tbuffer.done;
begin
  dispose(storage);
end;

procedure Tbuffer.put(var d;amount:word;var amountput:word);
var la,ls,lst:longint;
begin
  la:=amount;
  ls:=size;
  lst:=start;
  if (lst+la+ls)> smallbufsize then
  begin
    move(storage^[start],storage^[0],size);
    start:=0;
 end;

 move(d,storage^[start+size],amount);

  size:=size+amount;
  amountPut:=amount;
end;

procedure Tbuffer.get(var d;amount:word;var amountgot:word);
var i:word;
begin

  if amount>size then amount:=size;


  move(storage^[start],d,amount);
  start:=start+amount;
  size:=size-amount;

  amountGot:=amount;
end;


procedure Tbuffer.restart;
begin
  start:=0;
  size:=0;

end;

begin
end.



