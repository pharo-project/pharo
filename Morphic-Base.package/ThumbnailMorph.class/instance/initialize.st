initialization
initialize
	"Initialize the receiver, obeying a #nominalExtent property if I have one"
	| anExtent |
	
	super initialize.
	anExtent := self valueOfProperty: #nominalExtent ifAbsent: [25@25].
	self extent: anExtent