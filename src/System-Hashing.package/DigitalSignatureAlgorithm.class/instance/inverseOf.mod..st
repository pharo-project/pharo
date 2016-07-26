large integer arithmetic
inverseOf: x mod: n
	"Answer the inverse of x modulus n. That is, the integer y such that (x * y) \\ n is 1. Both x and n must be positive, and it is assumed that x < n and that x and n are integers."
	"Details: Use the extended Euclidean algorithm, Schneier, p. 247."

	| v u u1 u2 u3 t1 t2 t3 tmp |
	((x <= 0) or: [n <= 0]) ifTrue: [self error: 'x and n must be greater than zero'].
	x >= n ifTrue: [self error: 'x must be < n'].

	v := x.
	u := n.
	(x even and: [n even]) ifTrue: [self error: 'no inverse'].

	u1 := 1. u2 := 0. u3 := u.
	t1 := v. t2 := u - 1. t3 := v.
	[	[u3 even ifTrue: [
			((u1 odd) or: [u2 odd]) ifTrue: [
				u1 := u1 + v.
				u2 := u2 + u].
			u1 := u1 bitShift: -1.
			u2 := u2 bitShift: -1.
			u3 := u3 bitShift: -1].
		((t3 even) or: [u3 < t3]) ifTrue: [
			tmp := u1. u1 := t1. t1 := tmp.
			tmp := u2. u2 := t2. t2 := tmp.
			tmp := u3. u3 := t3. t3 := tmp].
		u3 even and: [u3 > 0]] whileTrue: ["loop while u3 is even"].

		[((u1 < t1) or: [u2 < t2]) and: [u1 > 0]] whileTrue: [
			u1 := u1 + v.
			u2 := u2 + u].
	
		u1 := u1 - t1.
		u2 := u2 - t2.
		u3 := u3 - t3.
		t3 > 0] whileTrue: ["loop while t3 > 0"].

	[u1 >= v and: [u2 >= u]] whileTrue: [
		u1 := u1 - v.
		u2 := u2 - u].

	u3 = 1 ifFalse: [self error: 'no inverse'].
	^ u - u2
