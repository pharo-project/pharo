instance creation
fromString: aString 
	"Answer an instance of me that is a copy of the argument, aString."

	| inst |
	(aString isMemberOf: self) ifTrue: [
		^ aString copy.
	].
	inst := self new: aString size.
	1 to: aString size do: [:pos |
		inst basicAt: pos put: (aString basicAt: pos).
	].
	^ inst.
