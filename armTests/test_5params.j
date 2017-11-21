class Main {

	void main(){
		Int a; 
		Compute help;
		
		help = new Compute();
		a = help.add(1,2,3,4,5);
		
		println("\nSum of 5 params:"); 
		println(a);
		
	}
}

class Compute {

	Int x;
	Int y;
	Int z;

   Int add(Int a, Int b, Int c, Int d, Int e){
     return (a + b + c + d + e);
   }   
   
}