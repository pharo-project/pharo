accessing
externalObjects
	"Not really sure why this is protected, once called you are out of protection of the locks anyways, and any use of the object is dangerous...
	Only additions can potentially change the actual array in use though, so only lock that."
	^ProtectAdd critical: [self unprotectedExternalObjects].