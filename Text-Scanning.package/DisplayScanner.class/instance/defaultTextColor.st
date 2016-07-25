private
defaultTextColor
	defaultTextColor ifNil:[ defaultTextColor := Smalltalk ui theme textColor ].
	^defaultTextColor