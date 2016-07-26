tests - copy
testCopyNonEmptyWith
	"self debug: #testCopyNonEmptyWith"
	| res anElement |
	anElement := self elementToAdd .
	res := self nonEmpty copyWith: anElement.
	"here we do not test the size since for a non empty set we would get a problem.
	Then in addition copy is not about duplicate management. The element should 
	be in at the end."
	self assert: (res includes: (anElement value)).
	self nonEmpty do: [ :each | res includes: each ]