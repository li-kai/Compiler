Parsing...
armTests/min_vector.j
======= IR3 Program =======

======= CData3 ======= 

class Solution{
}

class Vector{
  Int x;
}

=======  CMtd3 ======= 

void main(Solution this){
  Vector i;
  Int _t0;
  i=new Vector();
  i.x=1;
  _t0=i.x;
  println(_t0);
}


======= End of IR3 Program =======

start
0
exit
KEY: start VALUE: 0,start
KEY: start VALUE: start
KEY: start VALUE: 
KEY: start VALUE: start
KEY: 0 VALUE: exit
KEY: 0 VALUE: 
KEY: 0 VALUE: exit
KEY: exit VALUE: 
id3: _t0 reg: v1
id3: i reg: v1
FOUND REGISTER v1 FOR ID i
FOUND REGISTER v1 FOR ID i
.data
	
	L1:
	.asciz "%i\n"
	
.text
	
.global main
	
main:
	stmfd sp!,{fp,lr,v1,v2,v3,v4,v5}
	add fp,sp,#24
	sub sp,fp,#36
	str a1,[fp,#-36]
	mov a1,#4
	bl _Znwj(PLT)
	str a1,[fp,#-28]
	ldr v1,[fp,#-28]
	ldr a1,[v1,#0]
	ldr a2,[fp,#-28]
	str a1,[a2,#0]
	ldr v1,[fp,#-28]
	ldr a1,[v1,#0]
	str a1,[fp,#-32]
	ldr a1,=L1
	ldr a2,[fp,#-32]
	bl printf(PLT)
	
.main_exit:
	sub sp,fp,#24
	ldmfd sp!,{fp,pc,v1,v2,v3,v4,v5}
	
