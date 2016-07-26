private
reificationsFor: aBlockOrBoolean
	(aBlockOrBoolean isKindOf: Boolean) ifTrue: [ ^#() ].
	(aBlockOrBoolean isKindOf: RFCondition) ifTrue: [^aBlockOrBoolean reifications].
	^aBlockOrBoolean argumentNames.