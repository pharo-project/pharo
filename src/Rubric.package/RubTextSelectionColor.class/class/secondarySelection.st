default accessing
secondarySelection
	^ SecondarySelection
		ifNil: [ 
			SecondarySelection := self new
				colorBlock: [ 
							self theme currentSettings haveSecondarySelectionTextColor
								ifTrue: [ self theme currentSettings secondarySelectionTextColor ]
								ifFalse: [  ] ];
				backgroundColorBlock: [ self theme currentSettings secondarySelectionColor ];
				yourself ]