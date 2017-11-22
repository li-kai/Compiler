class Solution {
	Void main () {
		Vector i;
		Vector j;
		Int dotRes;

		i = new Vector();
		i.x = 1;
		j.y = 0;

		j = new Vector();
		j.x = i.y;
		j.y = i.x;

		dotRes = i.dotVector(j);
		println(dotRes);
		return;
	}
}

class Vector {
	Int x;
	Int y;

	Void addVector(Vector that) {
		this.x = this.x + that.x;
		this.y = this.y + that.y;
	}

	Void scaleVector(Int x) {
		this.x = this.x * x;
		this.y = this.y * x;
	}

	Int dotVector(Vector that) {
		Int result;
		result = result + this.x * that.x;
		result = result + this.y * that.y;
		return result;
	}

	Bool isEqual(Vector that) {
		return this.x == that.x && this.y == that.y;
	}
}
