Class {
	#name : #GTPlayPageFilePersistence,
	#superclass : #GTPlayPagePersistence,
	#category : #'GT-Playground'
}

{ #category : #private }
GTPlayPageFilePersistence >> deleteFromFileSystem [
	self fileReference ensureDelete
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
	self fileReference writeStreamDo: [ :stream | 
		stream 
			truncate; 
			nextPutAll: self page contentString ]
]
