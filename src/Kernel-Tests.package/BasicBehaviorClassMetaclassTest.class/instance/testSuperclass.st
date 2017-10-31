tests
testSuperclass
	"self debug: #testSuperclass"

	| s b |
	s := OrderedCollection new.
	b := [:cls | cls ifNotNil: [s add: cls. b value: cls superclass] ].
	b value: OrderedCollection.

	self assert: OrderedCollection allSuperclasses = s allButFirst.
	self assert: OrderedCollection withAllSuperclasses = s.