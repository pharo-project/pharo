Extension { #name : #String }

{ #category : #'*Filesystem-Core-converting' }
String >> asFileLocatorOrReference [

	^ FileLocator fromString: self ifNone: [ self asFileReference ]

]

{ #category : #'*Filesystem-Core-converting' }
String >> asFileReference [

	^ FileSystem disk referenceTo: self
]

{ #category : #'*Filesystem-Core-converting' }
String >> asPath [
	"convert myself to a path"
	"Examples:
		'.' asPath
		'~/Desktop' asPath
		'/home/foo' asPath
		'../../foo.bar' asPath"
	^ FileSystem disk resolve: self
]

{ #category : #'*Filesystem-Core-converting' }
String >> asPathWith: anObject [ 
	^ anObject pathFromString: self
]

{ #category : #'*Filesystem-Core-converting' }
String >> asResolvedBy: aFileSystem [
	^ aFileSystem resolveString: self
]
