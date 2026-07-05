Extension { #name : 'String' }

{ #category : '*FileSystem-Disk' }
String >> asFileLocatorOrReference [

	^ FileLocator fromString: self ifNone: [ self asFileReference ]
]

{ #category : '*FileSystem-Disk' }
String >> asFileReference [

	^ FileSystem disk referenceTo: self
]

{ #category : '*FileSystem-Disk' }
String >> asPath [
	"convert myself to a path"
	"Examples:
		'.' asPath
		'~/Desktop' asPath
		'/home/foo' asPath
		'../../foo.bar' asPath"
	^ FileSystem disk resolve: self
]
