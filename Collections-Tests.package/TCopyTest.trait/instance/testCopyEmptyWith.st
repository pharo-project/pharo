tests - copy
testCopyEmptyWith
	"self debug: #testCopyWith"
	| res anElement |
	anElement := self elementToAdd.
	res := self empty copyWith: anElement.
	self assert: res size = (self empty size + 1).
	self assert: (res includes: (anElement value))