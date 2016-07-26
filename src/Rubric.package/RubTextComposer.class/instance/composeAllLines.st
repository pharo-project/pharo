private composition
composeAllLines
	[ currCharIndex <= theText size and: [ currentY + defaultLineHeight <= theContainer bottom ] ]
		whileTrue: [ 
			(nowSliding
				ifTrue: [ self slideOneLineDown ]
				ifFalse: [ self composeOneLine ]) ifNil: [ ^ nil ] ]