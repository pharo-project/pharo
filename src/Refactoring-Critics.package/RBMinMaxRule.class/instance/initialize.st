initialization
initialize
	super initialize.
	self rewriteRule
		replace: '``@a < ``@b ifTrue: [``@a] ifFalse: [``@b]' with: '``@a min: ``@b';
		replace: '``@a <= ``@b ifTrue: [``@a] ifFalse: [``@b]' with: '``@a min: ``@b';
		replace: '``@a > ``@b ifTrue: [``@a] ifFalse: [``@b]' with: '``@a max: ``@b';
		replace: '``@a >= ``@b ifTrue: [``@a] ifFalse: [``@b]' with: '``@a max: ``@b';
		replace: '``@a < ``@b ifTrue: [``@b] ifFalse: [``@a]' with: '``@a max: ``@b';
		replace: '``@a <= ``@b ifTrue: [``@b] ifFalse: [``@a]' with: '``@a max: ``@b';
		replace: '``@a > ``@b ifTrue: [``@b] ifFalse: [``@a]' with: '``@a min: ``@b';
		replace:  '``@a >= ``@b ifTrue: [``@b] ifFalse: [``@a]' with: '``@a min: ``@b';
		replace: '`a < ``@b ifTrue: [`a := ``@b]' with: '`a := `a max: ``@b';
		replace: '`a <= ``@b ifTrue: [`a := ``@b]' with: '`a := `a max: ``@b';
		replace: '`a < ``@b ifFalse: [`a := ``@b]' with: '`a := `a min: ``@b';
		replace: '`a <= ``@b ifFalse: [`a := ``@b]' with: '`a := `a min: ``@b';
		replace: '`a > ``@b ifTrue: [`a := ``@b]' with: '`a := `a min: ``@b';
		replace: '`a >= ``@b ifTrue: [`a := ``@b]' with: '`a := `a min: ``@b';
		replace: '`a > ``@b ifFalse: [`a := ``@b]' with: '`a := `a max: ``@b';
		replace: '`a >= ``@b ifFalse: [`a := ``@b]' with: '`a := `a max: ``@b';
		replace: '``@b < `a ifTrue: [`a := ``@b]' with: '`a := `a min: ``@b';
		replace: '``@b <= `a ifTrue: [`a := ``@b]' with: '`a := `a min: ``@b';
		replace: '``@b < `a ifFalse: [`a := ``@b]' with: '`a := `a max: ``@b';
		replace: '``@b <= `a ifFalse: [`a := ``@b]' with: '`a := `a max: ``@b';
		replace: '``@b > `a ifTrue: [`a := ``@b]' with: '`a := `a max: ``@b';
		replace: '``@b >= `a ifTrue: [`a := ``@b]' with: '`a := `a max: ``@b';
		replace: '``@b > `a ifFalse: [`a := ``@b]' with: '`a := `a min: ``@b';
		replace: '``@b >= `a ifFalse: [`a := ``@b]' with: '`a := `a min: ``@b'