Extension { #name : #WriteStream }

{ #category : #'*CodeExport' }
WriteStream >> nextChunkPut: aString [
	"Append the argument, aString, to the receiver, doubling embedded ! terminators and adding a extra one"
 
	| string start bangIndex |
	string := aString asString.
	start := 1.
	[ (bangIndex := string indexOf: $! startingAt: start) = 0 ]
		whileFalse: [
			self next: bangIndex - start + 1 putAll: string startingAt: start.
			self nextPut: $!. "double it"
			start := bangIndex + 1 ].
	self next: string size - start + 1 putAll: string startingAt: start.
	string includesUnifiedCharacter ifTrue: [
		self nextPut: $!; nextPutAll: ']lang['.
		string writeLeadingCharRunsOn: self.
	].
	self nextPut: $!. "one extra"
	self flush
]

{ #category : #'*CodeExport' }
WriteStream >> timeStamp [
	"Append the current time to the receiver as a String."
	self nextChunkPut:	"double string quotes and !s"
		(String streamContents: [:s | Smalltalk timeStamp: s]) printString.
	self cr
]
