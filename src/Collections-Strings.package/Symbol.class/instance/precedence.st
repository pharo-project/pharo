accessing
precedence
	"Answer the receiver's precedence, assuming it is a valid Smalltalk
	message selector or 0 otherwise.  The numbers are 1 for unary,
	2 for binary and 3 for keyword selectors."

	self size = 0 ifTrue: [^ 0].
	(self first isLetter or: [ self first = $_ ]) ifFalse: [^ 2].
	self last = $: ifTrue: [^ 3].
	^ 1