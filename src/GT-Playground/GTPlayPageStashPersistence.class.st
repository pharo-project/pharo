Class {
	#name : #GTPlayPageStashPersistence,
	#superclass : #GTPlayPageFilePersistence,
	#category : #'GT-Playground'
}

{ #category : #accessing }
GTPlayPageStashPersistence >> fileDirectory [
	^ GTPlayBook stashDirectory
]

{ #category : #accessing }
GTPlayPageStashPersistence >> fileName [
	^ self page title, '-', self page creationTimeForFileName, '.', GTPlayPage fileExtension
]

{ #category : #testing }
GTPlayPageStashPersistence >> isStash [
	^ true
]
