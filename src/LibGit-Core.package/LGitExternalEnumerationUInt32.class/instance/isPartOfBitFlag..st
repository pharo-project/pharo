testing
isPartOfBitFlag: anInteger
	"A bit flag may be composed of several values, where every bit
	represents the state of one flag. Therefore, a 2 bit value can
	represent 2 different flags, for instance. The #bitAnd: operation
	will mask all other flags.
	
	For some reason the libgit2 folks thought it would make sense to
	use flags of value 0 in bit flags. That forces one to check for zero
	(on the entire word)	explicitly. WTF."
	^ self value isZero
		ifTrue: [ anInteger isZero ]
		ifFalse: [ (self value bitAnd: anInteger) = self value ]