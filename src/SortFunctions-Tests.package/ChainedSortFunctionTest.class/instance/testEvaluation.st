tests
testEvaluation

	| function |
	function := #first ascending , [:a | a at: 2] descending , #last ascending.
	"first different"
	self assert: (function value: #(1 2 3) value: #(2 2 3)).
	self deny: (function value: #(3 2 3) value: #(2 2 3)).
	"first the same"
	self assert: (function value: #(1 2 3) value: #(1 1 3)).
	self assert: (function value: #(1 2 2) value: #(1 1 3)).
	self assert: (function value: #(1 2 3) value: #(1 1 2)).
	self deny: (function value: #(1 2 3) value: #(1 3 3)).
	self deny: (function value: #(1 2 2) value: #(1 3 3)).
	self deny: (function value: #(1 2 3) value: #(1 3 2)).
	"first and second the same"
	self assert: (function value: #(1 2 3) value: #(1 2 3)).
	self assert: (function value: #(1 2 2) value: #(1 2 3)).
	self deny: (function value: #(1 2 4) value: #(1 2 3))