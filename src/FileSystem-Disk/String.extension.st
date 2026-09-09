Extension { #name : 'String' }

{ #category : '*FileSystem-Disk' }
String >> asFileLocatorOrReference [

	^ FileLocator fromString: self ifNone: [ self asFileReference ]
]

{ #category : '*FileSystem-Disk' }
String >> asFileName [
	"Answer a String made up from the receiver that is an acceptable file name."

	| string checkedString |
	string := FileSystem disk checkName: self fixErrors: true.
	checkedString := FilePathEncoder encode: string.
	^ FilePathEncoder decode: checkedString
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
