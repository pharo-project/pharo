updating
okToChange
	| answer |
	self sourceTextModel ifNil: [ ^ true ].
	
	(self sourceTextModel canDiscardEdits and: [ 
		self sourceTextModel getString trimBoth = sourceTextWidget text asString trimBoth ]
	) ifTrue: [ ^ true ].

	answer := self promptForCancel.
	answer ifNotNil: [
			answer
				 ifTrue: [ self acceptSourceCodeFromView ]
				ifFalse: [ self sourceTextModel cancelEdits ] ].
	^ self sourceTextModel canDiscardEdits