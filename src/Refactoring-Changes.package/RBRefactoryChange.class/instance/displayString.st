printing
displayString
	^name isNil ifTrue: [self changeString] ifFalse: [name]