Class {
	#name : #GTPlayPageFilePersistence,
	#superclass : #GTPlayPagePersistence,
	#category : #'GT-Playground'
}

{ #category : #private }
GTPlayPageFilePersistence >> deleteFromFileSystem [

	| aFileReference |
	
	aFileReference := self fileReference.
	(aFileReference notNil and: [ aFileReference fileSystem isWritable ]) ifTrue: [ 
		aFileReference ensureDelete ]
]

{ #category : #accessing }
GTPlayPageFilePersistence >> fileDirectory [
	^ self subclassResponsibility 
]

{ #category : #accessing }
GTPlayPageFilePersistence >> fileName [
	self subclassResponsibility
]

{ #category : #'accessing-dynamic' }
GTPlayPageFilePersistence >> fileReference [

	self fileDirectory ifNil: [ 
		"the directory was not created (read-only filesystem etc.)"
		^ nil ].
	
	^ self fileDirectory / self fileName
]

{ #category : #'accessing-dynamic' }
GTPlayPageFilePersistence >> fileReferenceOrNil [

	self fileDirectory ifNil: [ 
		"the directory was not created (read-only filesystem etc.)"
		^ nil ].
	
	^ self fileDirectory / self fileName
]

{ #category : #actions }
GTPlayPageFilePersistence >> save [
	self page content isEmpty
		ifTrue: [ self deleteFromFileSystem ]
		ifFalse: [ self writeToFileSystem ]
]

{ #category : #private }
GTPlayPageFilePersistence >> writeToFileSystem [

	| aFileReference |
	
	aFileReference := self fileReferenceOrNil.
	(aFileReference notNil and: [ aFileReference fileSystem isWritable ]) ifTrue: [
		aFileReference writeStreamDo: [ :stream | 
			stream 
				truncate; 
				nextPutAll: self page contentString ]]
]
