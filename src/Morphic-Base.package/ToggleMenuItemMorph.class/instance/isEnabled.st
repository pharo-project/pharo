accessing
isEnabled
	"Answer whether the item is enabled."
	
	|state|
	self enablementSelector ifNil: [^super isEnabled].
	state := self enablementSelector isSymbol
		ifTrue: [self target perform: self enablementSelector]
		ifFalse: [self enablementSelector value].
	self isEnabled: state.
	^state