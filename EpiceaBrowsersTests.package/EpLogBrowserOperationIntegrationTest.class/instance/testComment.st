tests
testComment
	| anEntry |
	classFactory newClass.

	anEntry := monitor log head.
	
	monitor logEntryCommented:
		(EpLogEntriesComment 
			entryReferences: { monitor log referenceTo: anEntry }
			oldComment: ''
			newComment: '42').
			
	self assert: (self countLogEventsWith: EpLogEntriesComment) equals: 1.
	
	monitor log 
		commentAt: anEntry
		ifAbsent: [ self fail: 'it should answer the comment' ].
		
	monitor log 
		commentAt: anEntry
		ifPresent: [ :comment | self assert: comment equals: '42' ].