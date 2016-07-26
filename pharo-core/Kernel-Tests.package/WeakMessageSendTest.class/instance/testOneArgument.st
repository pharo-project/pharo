tests
testOneArgument
	"self run: #testOneArgument"	

	| m |
	m := WeakMessageSend
		receiver: Array
		selector: #with:
		argument: 1.
	Smalltalk garbageCollectMost.
	self assert: (m value  = { 1 })
