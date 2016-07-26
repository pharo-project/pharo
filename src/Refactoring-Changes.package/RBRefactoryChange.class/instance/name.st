accessing
name
	^name isNil ifTrue: [self changeString] ifFalse: [name]