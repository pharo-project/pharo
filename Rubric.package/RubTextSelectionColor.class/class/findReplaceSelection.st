default accessing
findReplaceSelection
	^ FindReplaceSelection
		ifNil: [ 
			FindReplaceSelection := self new
				colorBlock: [ 
							self theme currentSettings haveFindReplaceSelectionTextColor
								ifTrue: [ self theme currentSettings findReplaceSelectionTextColor ]
								ifFalse: [  ] ];
				backgroundColorBlock: [ self theme currentSettings findReplaceSelectionColor ];
				yourself ]