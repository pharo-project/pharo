*CodeExport
nextChunkPut: aString
	"Append the argument, aString, to the receiver, doubling embedded terminators."
 
	| i remainder terminator | 
	terminator := $!.
	remainder := aString asString.
	[(i := remainder indexOf: terminator) = 0] whileFalse:
		[self nextPutAll: (remainder copyFrom: 1 to: i).
		self nextPut: terminator.  "double imbedded terminators"
		remainder := remainder copyFrom: i+1 to: remainder size].
	self nextPutAll: remainder. 
	aString asString includesUnifiedCharacter ifTrue: [
		self nextPut: terminator.
		self nextPutAll: ']lang['.
		aString asString writeLeadingCharRunsOn: self.
	].
	self nextPut: terminator.
	self flush.