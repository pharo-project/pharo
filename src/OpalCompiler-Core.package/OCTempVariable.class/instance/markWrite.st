escaping
markWrite

	"if an escaping var is wrote to later, it needs to be remote"
	self isEscaping 
		ifTrue: [self markEscapingWrite].
	super markWrite.