reporting
reportWin32VMConfig: aStream
	| exePath iniPath |
	self header: 'VM Configuration' on: aStream.
	exePath := Smalltalk vm vmFileName.
	iniPath := (exePath copyUpToLast: $.), '.ini'.
	aStream nextPutAll: iniPath; cr.
	self writeContentsSafelyFromFile: iniPath on: aStream.
