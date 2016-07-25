accessing
isLocked
	"answer whether the receiver is Locked"
	extension ifNil: [^ false].
	^ extension locked