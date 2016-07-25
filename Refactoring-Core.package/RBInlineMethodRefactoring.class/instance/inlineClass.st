transforming
inlineClass
	^inlineClass isNil 
		ifTrue: 
			[inlineClass := (sourceMessage receiver name = 'super' 
						ifTrue: [class superclass]
						ifFalse: [class]) whoDefinesMethod: self inlineSelector]
		ifFalse: [inlineClass]