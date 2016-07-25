initialization
initialize
	super initialize.
	self rewriteRule
		replace: '``@a >= ``@b and: [``@a <= ``@c]' with: '``@a between: ``@b and: ``@c';
		replace: '``@a >= ``@b & (``@a <= ``@c)' with: '``@a between: ``@b and: ``@c';
		replace: '``@b <= ``@a and: [``@a <= ``@c]' with: '``@a between: ``@b and: ``@c';
		replace: '``@b <= ``@a & (``@a <= ``@c)' with: '``@a between: ``@b and: ``@c';
		replace: '``@a <= ``@c and: [``@a >= ``@b]' with: '``@a between: ``@b and: ``@c';
		replace: '``@a <= ``@c & (``@a >= ``@b)' with: '``@a between: ``@b and: ``@c';
		replace: '``@c >= ``@a and: [``@a >= ``@b]' with: '``@a between: ``@b and: ``@c';
		replace: '``@c >= ``@a & (``@a >= ``@b)' with: '``@a between: ``@b and: ``@c';
		replace: '``@a >= ``@b and: [``@c >= ``@a]' with: '``@a between: ``@b and: ``@c';
		replace: '``@a >= ``@b & (``@c >= ``@a)' with: '``@a between: ``@b and: ``@c';
		replace: '``@b <= ``@a and: [``@c >= ``@a]' with: '``@a between: ``@b and: ``@c';
		replace: '``@b <= ``@a & (``@c >= ``@a)' with: '``@a between: ``@b and: ``@c';
		replace: '``@a <= ``@c and: [``@b <= ``@a]' with: '``@a between: ``@b and: ``@c';
		replace: '``@a <= ``@c & (``@b <= ``@a)' with: '``@a between: ``@b and: ``@c';
		replace: '``@c >= ``@a and: [``@b <= ``@a]' with: '``@a between: ``@b and: ``@c';
		replace: '``@c >= ``@a & (``@b <= ``@a)' with: '``@a between: ``@b and: ``@c'