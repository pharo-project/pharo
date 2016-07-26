close and dup2
defineActionsForReadStream: aReadStream dupTo: aFileDescriptor
	"Sets all needed actions (close() and dup2()) for a aReadStream (stdin) FOR THE CHILD.
	"
	aReadStream ifNotNil: [ 
		aReadStream ossIsPipe 
			ifTrue: [  
				"For pipes, in the child, we close write end (the child will read), 
				then dup2 the reader to stdin, and then close the reader too. "
				self posixSpawnFileActionsAddClose: (self systemAccessor primUnixFileNumber: aReadStream writer basicFileID).
				self posixSpawnFileActionsAddDup2: (self systemAccessor primUnixFileNumber: aReadStream reader basicFileID) newFileDescritpion: aFileDescriptor.
				self posixSpawnFileActionsAddClose: (self systemAccessor primUnixFileNumber: aReadStream reader basicFileID).	
			] 
			ifFalse: [ 
				| fileDesc |
				fileDesc := self getFdFromStream: aReadStream. 
				"For regular files, in the child, we dup2 the reader to stdin, and then close the reader too. "
				self posixSpawnFileActionsAddDup2: fileDesc newFileDescritpion: aFileDescriptor.
				self posixSpawnFileActionsAddClose: fileDesc.
			
			]
	]