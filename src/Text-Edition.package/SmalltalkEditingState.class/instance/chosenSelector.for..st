selector-chooser
chosenSelector: aSelector for: aTextMorph
	| newStart replacement  caret prevSelection |
	self closeSelectorChooser.
	aSelector ifNil: [^ self].
	aTextMorph editor selectPrecedingIdentifier.
	prevSelection := aTextMorph selection.
	newStart := aTextMorph editor startIndex.
	caret := aTextMorph editor stopIndex.
	replacement := String streamContents: [:strm | | kwds |
		1 to: (kwds := aSelector keywords) size 
		do: [:i | strm nextPutAll: (kwds at: i).
			i = 1 ifTrue: [caret := newStart + strm contents size + 1].
			strm nextPutAll: (i < kwds size ifTrue: ['  '] ifFalse: [' '])]].
	prevSelection asString = replacement trimBoth 
		ifTrue: [aTextMorph flash]
		ifFalse: [aTextMorph replaceSelectionWith: replacement]. 
	aTextMorph selectFrom: caret to: caret - 1.
	(aTextMorph respondsTo: #editView) ifTrue: [aTextMorph editView selectionInterval: aTextMorph editor selectionInterval].
