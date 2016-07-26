close and dup2
defineActionsForWriteStream: aWriteStream dupTo: aFileDescriptor
	"Sets all needed actions (close() and dup2()) for a aWriteStream (stdout or stderr) FOR THE CHILD
	"
	aWriteStream ifNotNil: [ 
		aWriteStream ossIsPipe 
			ifTrue: [  
				"For pipes, in the child, we close read end (the child will write), 
				then dup2 the writer to stderr or stdout, and then close the writer too. "
				self posixSpawnFileActionsAddClose: (self systemAccessor primUnixFileNumber: aWriteStream reader basicFileID).
				self posixSpawnFileActionsAddDup2: (self systemAccessor primUnixFileNumber: aWriteStream writer basicFileID) newFileDescritpion: aFileDescriptor.
				self posixSpawnFileActionsAddClose: (self systemAccessor primUnixFileNumber: aWriteStream writer basicFileID).	
			] 
			ifFalse: [ 
				| fileDesc |
				fileDesc := self getFdFromStream: aWriteStream. 
				"For regular files, in the child, we dup2 the writer to stdout or stderr, and then close the writer too. "
				self posixSpawnFileActionsAddDup2: fileDesc newFileDescritpion: aFileDescriptor.
				self posixSpawnFileActionsAddClose: fileDesc.
			
			]
	]