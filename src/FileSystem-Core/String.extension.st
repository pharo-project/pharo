Extension { #name : #String }

{ #category : #'*filesystem-core-converting' }
String >> asFileReference [

	^ FileSystem disk referenceTo: self
]

{ #category : #'*filesystem-core-converting' }
String >> asPath [
	"convert myself to a path"
	"Examples:
		'.' asPath
		'~/Desktop' asPath
		'/home/foo' asPath
		'../../foo.bar' asPath"
	^ FileSystem disk resolve: self
]

{ #category : #'*filesystem-core-converting' }
String >> asPathWith: anObject [ 
	^ anObject pathFromString: self
]

{ #category : #'*filesystem-core-converting' }
String >> asResolvedBy: aFileSystem [
	^ aFileSystem resolveString: self
]
