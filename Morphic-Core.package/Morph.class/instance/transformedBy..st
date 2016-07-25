geometry
transformedBy: aTransform
	aTransform isIdentity ifTrue:[^self].
	aTransform isPureTranslation ifTrue:[
		^self position: (aTransform localPointToGlobal: self position).
	].
	^self addFlexShell transformedBy: aTransform