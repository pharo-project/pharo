compiler support
callingConventionFor: aString
	"Return the constant describing the calling convention for the given string specification or nil if unknown."
	aString = 'cdecl:' ifTrue:[^self callTypeCDecl].
	aString = 'apicall:' ifTrue:[^self callTypeAPI].
	^nil