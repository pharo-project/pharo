Class {
	#name : #GTPlayPageCachePersistence,
	#superclass : #GTPlayPageFilePersistence,
	#category : #'GT-Playground'
}

{ #category : #accessing }
GTPlayPageCachePersistence >> fileDirectory [
	^ GTPlayBook cacheDirectory
]

{ #category : #accessing }
GTPlayPageCachePersistence >> fileName [
	^ self page creationTimeForFileName, '.' , GTPlayPage fileExtension
]
