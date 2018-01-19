Class {
	#name : #EpCommentTest,
	#superclass : #EpLogBrowserOperationFactoryTest,
	#category : #'EpiceaBrowsers-Tests-Integration'
}

{ #category : #tests }
EpCommentTest >> testComment [

	| presentComment |
	classFactory newClass.
	self setHeadAsInputEntry.
	
	monitor logEntryCommented:
		(EpLogEntriesComment 
			entryReferences: { monitor log referenceTo: inputEntry }
			oldComment: ''
			newComment: '42').
			
	self assert: (self countLogEventsWith: EpLogEntriesComment) equals: 1.
	
	monitor log 
		commentAt: inputEntry
		ifAbsent: [ self fail: 'it should answer the comment' ].
		
	presentComment := nil.
	monitor log 
		commentAt: inputEntry
		ifPresent: [ :comment | presentComment := comment ].
	self assert: presentComment equals: '42'.

]
