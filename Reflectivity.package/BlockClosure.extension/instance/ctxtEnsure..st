*Reflectivity
ctxtEnsure: aBlock
	"Evaluate a termination block after evaluating the receiver, regardless of whether the receiver's evaluation completes."

	<primitive: 198>
	| returnValue b |
	returnValue := self value.
	aBlock isNil
		ifFalse: [ 
			"nil out aBlock temp before evaluating aBlock so it is not executed again if aBlock remote returns"
			b := aBlock.
			thisContext tempAt: 1 put: nil.
			b value ].
	^ returnValue