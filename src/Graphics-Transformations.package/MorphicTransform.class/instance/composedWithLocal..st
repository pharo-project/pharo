composing
composedWithLocal: aTransform
	aTransform isIdentity ifTrue:[^self].
	self isIdentity ifTrue:[^aTransform].
	aTransform isMorphicTransform ifFalse:[^super composedWithLocal: aTransform].
	self isPureTranslation ifTrue:[
		^aTransform withOffset: aTransform offset + self offset].
	aTransform isPureTranslation ifTrue:[
		^self withOffset: (self localPointToGlobal: aTransform offset negated) negated].
	^super composedWithLocal: aTransform.