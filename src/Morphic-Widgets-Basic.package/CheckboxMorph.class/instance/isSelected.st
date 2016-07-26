protocol
isSelected
	"Answer whether the receiver is selected."

	self model ifNil: [^false].
	^self model perform: (self getStateSelector ifNil: [^false])