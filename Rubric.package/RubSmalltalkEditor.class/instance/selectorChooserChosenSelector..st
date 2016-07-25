basic-completion
selectorChooserChosenSelector: aSelector
	| newStart replacement caret prevSelection |
	self selectorChooserClose.
	aSelector ifNil: [ ^ self ].
	self selectPrecedingIdentifier.
	prevSelection := textArea selection.
	newStart := self startIndex.
	caret := self stopIndex.
	replacement := String
		streamContents: [ :strm | 
			| kwds |
			1 to: (kwds := aSelector keywords) size do: [ :i | 
				strm nextPutAll: (kwds at: i).
				i = 1
					ifTrue: [ caret := newStart + strm contents size + 1 ].
				strm
					nextPutAll:
						(i < kwds size
							ifTrue: [ '  ' ]
							ifFalse: [ ' ' ]) ] ].
	prevSelection asString = replacement trimBoth
		ifTrue: [ textArea flash ]
		ifFalse: [ 
			self replaceSelectionWith: replacement.
			self selectFrom: caret to: caret - 1 ]