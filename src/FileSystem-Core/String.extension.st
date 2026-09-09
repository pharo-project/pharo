Extension { #name : 'String' }

{ #category : '*Filesystem-Core-converting' }
String >> asPathWith: anObject [
	^ anObject pathFromString: self
]

{ #category : '*Filesystem-Core-converting' }
String >> asResolvedBy: aFileSystem [
	^ aFileSystem resolveString: self
]
