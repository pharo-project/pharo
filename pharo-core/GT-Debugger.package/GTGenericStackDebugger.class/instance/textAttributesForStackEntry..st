printing/formatting
textAttributesForStackEntry: aContext

	(self selectedContext isNil or: self class enableStackColoring not) 
		ifTrue: [ ^ {TextColor color: Smalltalk ui theme textColor} ].

	^ Smalltalk ui theme styleContext: aContext from: self.