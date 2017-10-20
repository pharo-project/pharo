tests - creation
testWith
	"self debug: #testWith"
	
	| aCol anElement |
	anElement := self collectionMoreThan5Elements anyOne.
	aCol := self collectionClass with: anElement.
	self assert: (aCol includes: anElement).