code generation
reserve: encoder 
	"If this is a yet unused literal of type -code, reserve it."

	code < 0 ifTrue: [code := self code: (index := encoder litIndex: key) type: 0 - code]