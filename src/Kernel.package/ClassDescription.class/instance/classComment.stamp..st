filein/out
classComment: aString stamp: aStamp
	"Store the comment, aString or Text or RemoteString, associated with the class we are organizing.  Empty string gets stored only if had a non-empty one before."

	| pointer header oldCommentRemoteString oldComment oldStamp preamble |
	oldComment := self organization classComment.
	oldStamp := self organization commentStamp.

	aString string = oldComment string ifTrue: [ ^ self ].

	aString isRemoteString ifTrue: [
		SystemAnnouncer uniqueInstance 
			class: self 
			oldComment: oldComment 
			newComment: aString string 
			oldStamp: oldStamp 
			newStamp: aStamp.
		^ self organization classComment: aString stamp: aStamp].

	oldCommentRemoteString := self organization commentRemoteString.
	pointer := oldCommentRemoteString 
			ifNil: [0] 
			ifNotNil: [oldCommentRemoteString sourcePointer].
	
	preamble := String streamContents: [ :file |
		file cr; nextPut: $!.
		header := String streamContents: [:strm | 
			strm 
				nextPutAll: self name;
				nextPutAll: ' commentStamp: '.
			aStamp storeOn: strm.
			strm nextPutAll: ' prior: '; nextPutAll: pointer printString ].
		file nextChunkPut: header; cr ].

	SourceFiles
		writeSource: aString
		preamble: preamble
		onSuccess: [ :newSourcePointer | 
			self organization 
				classComment: (SourceFiles remoteStringAt: newSourcePointer) 
				stamp: aStamp ]
		onFail: [ "ignore" ].
	
	SystemAnnouncer uniqueInstance 
		class: self 
		oldComment: oldComment 
		newComment: aString 
		oldStamp: oldStamp 
		newStamp: aStamp