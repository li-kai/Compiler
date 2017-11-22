class Main {
	Void main () {
		Facto f;
		Int res;

		f = new Facto();
		res = f.fact(10);

		println(res);		


		return;
	}
}

class Facto { 
	
	Int fact (Int n) {
		Facto f;
		if (n <= 1) {
			return 1;

		} else {
			f = new Facto();
			return n * f.fact(n - 1);
		}
	
	}
}