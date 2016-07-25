comparing
= anEvent
	anEvent isMorphicEvent ifFalse:[^false].
	^self type = anEvent type