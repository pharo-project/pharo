tests
testEmptyError
	| aStack |
	aStack := Stack new.
	self should: [ aStack top ] raise: Error.
	self should: [ aStack pop ] raise: Error.
	aStack push: 'element'.
	aStack top.
	aStack pop.	"The stack is empty again due to previous pop"
	self should: [ aStack top ] raise: Error.
	self should: [ aStack pop ] raise: Error