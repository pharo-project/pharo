accessing
stamp

	^self hasComment ifTrue:[ self comment stamp ] ifFalse:[ nil ]