comparing
= aMorphicEvent
	super = aMorphicEvent ifFalse:[^false].
	buttons = aMorphicEvent buttons ifFalse: [^ false].
	keyValue = aMorphicEvent keyValue ifFalse: [^ false].
	^ true
