wrapping
wrapItem: anItem index: anIndex
	"Use the wrapSelector to get the text or string representation of a list item."
	
	^ self wrapSelector 
		ifNil: [ anItem asStringOrText]
		ifNotNil: [:selector | 
			selector numArgs == 0
				ifTrue: [ anItem perform: selector ]
				ifFalse: [ self model perform: selector withEnoughArguments: { anItem. anIndex } ]].