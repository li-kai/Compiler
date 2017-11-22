class Main {
	Void main () {
		Fibo f;
		Int res;

		f = new Fibo();
		res = f.fib(6);

		println("");		


		return;
	}
}

class Fibo { 
	
	Int fib (Int n) {
		Fibo f;
		if (n <= 1) {
			return n;

		} else {
			f = new Fibo();
			return f.fib(n - 2) + f.fib(n - 1);
		}
	
	}
}