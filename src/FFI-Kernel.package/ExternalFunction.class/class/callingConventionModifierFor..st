compiler support
callingConventionModifierFor: aString
	"Return the constant describing the calling convention modifier for the given string specification or nil if unknown."
	aString = 'threaded' ifTrue:[^FFICallFlagThreaded].
	^nil