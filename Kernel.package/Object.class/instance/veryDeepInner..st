copying
veryDeepInner: deepCopier
	"No special treatment for inst vars of my superclasses.  Override when some need to be weakly copied.  Object>>veryDeepCopyWith: will veryDeepCopy any inst var whose class does not actually define veryDeepInner:"

	"avoid to use me we will deprecate it in the future"