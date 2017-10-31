tests - copy
testCopyEmptyWith
	"self debug: #testCopyEmptyWith"

	| res anElement |
	anElement := self elementToAdd.
	res := self empty copyWith: anElement.
	self assert: res size equals: self empty size + 1.
	self assert: (res includes: anElement).
	self deny: (empty includes: anElement)