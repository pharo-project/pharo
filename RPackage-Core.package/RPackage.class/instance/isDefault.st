testing
isDefault
	"Return whether the receiver is the default package, i.e. the one used when nothing else is specified."
	^ self name = self class defaultPackageName