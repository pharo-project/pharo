as yet unclassified
appendToCompositeText: aText
	"If appropriate append the relevant src or dst text to the given text."
	
	self selectionState == #src ifTrue: [^aText append: self src text].
	self selectionState == #dst ifTrue: [^aText append: self dst text].
	self selectionState == #both ifTrue: [
		^aText
			append: self src text;
			append: self dst text]