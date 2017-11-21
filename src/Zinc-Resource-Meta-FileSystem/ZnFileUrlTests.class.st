Class {
	#name : #ZnFileUrlTests,
	#superclass : #TestCase,
	#category : #Zinc-Resource-Meta-FileSystem
}

{ #category : #testing }
ZnFileUrlTests >> testAsFileReference [
	| fileUrl fileReference |
	fileUrl := 'file:///foo/bar/test.txt' asZnUrl.
	fileReference := fileUrl asFileReference.
	self assert: fileReference basename equals: 'test.txt'.
	self assert: fileReference pathSegments asArray equals: #( 'foo' 'bar' 'test.txt' ).
	self assert: fileReference isAbsolute 
]

{ #category : #testing }
ZnFileUrlTests >> testAsZnUrl [
	| fileReference fileUrl |
	fileReference := FileLocator root / 'foo' / 'bar' / 'test.txt'.
	fileUrl := fileReference asZnUrl.
	self assert: fileUrl isFile.
	self assert: fileUrl directory equals: 'foo/bar'.
	self assert: fileUrl file equals: 'test.txt'.
	self assert: fileUrl pathSegments asArray equals: #( 'foo' 'bar' 'test.txt').
	self assert: fileUrl pathPrintString equals: '/foo/bar/test.txt'.
	self assert: fileUrl printString equals: 'file:///foo/bar/test.txt'.
	self deny: fileUrl hasHost.
	self deny: fileUrl hasPort.
	self deny: fileUrl hasQuery.
	self deny: fileUrl hasFragment
]

{ #category : #testing }
ZnFileUrlTests >> testImage [
	self assert: ZnUrl image asFileReference exists
]

{ #category : #testing }
ZnFileUrlTests >> testRelative [
	| fileReference fileUrl |
	fileReference := 'foo/bar/test.txt' asFileReference.
	self assert: fileReference isRelative.
	fileUrl := fileReference asZnUrl.
	self assert: fileUrl isFile.
	self assert: fileUrl pathSegments size >= fileReference pathSegments size.
	self assert: (fileUrl pathSegments asArray endsWith: fileReference pathSegments asArray)
]

{ #category : #testing }
ZnFileUrlTests >> testRetrieveContents [
	| fileReference contents url |
	SystemVersion current major < 3 ifTrue: [ self skip ].
	fileReference := ('file-{1}.txt' format: {(1000 atRandom)}) asFileReference.
	contents := 'Testing {1} and counting.{2}' format: {(1000 atRandom). (String crlf)}.
	fileReference ensureDelete.
	fileReference writeStreamDo: [ :stream | stream nextPutAll: contents ].
	url := fileReference asZnUrl.
	self assert: url isFile.
	self assert: url retrieveContents equals: contents.
	fileReference ensureDelete
]

{ #category : #testing }
ZnFileUrlTests >> testRoot [
	| fileUrl fileReference |
	fileReference := FileLocator root.
	self assert: fileReference isRoot.
	fileUrl := fileReference asZnUrl.
	self assert: fileUrl isFile.
	self assert: fileUrl isSlash
]

{ #category : #testing }
ZnFileUrlTests >> testSlash [
	| fileUrl fileReference |
	fileUrl := 'file:///' asZnUrl.
	self assert: fileUrl isSlash.
	self assert: fileUrl isFile.
	fileReference := fileUrl asFileReference.
	self assert: fileReference isRoot
]

{ #category : #testing }
ZnFileUrlTests >> testSpaces [
	self 
		assert: 'file:///foo%20bar/test.st' asZnUrl asFileReference 
		equals: FileLocator root / 'foo bar' / 'test.st'.
	self 
		assert: (FileLocator root / 'foo bar' / 'test.st') asZnUrl 
		equals: 'file:///foo%20bar/test.st' asZnUrl
]

{ #category : #testing }
ZnFileUrlTests >> testTrailingSlash [
	| fileReference1 fileReference2 fileUrl1 fileUrl2 |
	
	fileReference1 := '/foo/bar' asFileReference.
	fileReference2 := '/foo/bar/' asFileReference.
	self assert: fileReference1 equals: fileReference2.
	self assert: fileReference1 asZnUrl equals: fileReference2 asZnUrl.
	
	fileUrl1 := 'file:///foo/bar' asZnUrl.
	fileUrl2 := 'file:///foo/bar/' asZnUrl.
	self deny: fileUrl1 = fileUrl2.
	self assert: fileUrl1 asFileReference equals: fileUrl2 asFileReference
]

{ #category : #testing }
ZnFileUrlTests >> testWindowsDriveNamesInFileUrl [
	"Note the 3 slashes: 2 are from the scheme, file://, 1 is from the empty host and starts the absolute path"
	
	SystemVersion current major < 3 ifTrue: [ self skip ].
	self 
		assert: 'file:///C:/Programs/notepad.exe' asUrl asFileReference
		equals: FileLocator C / 'Programs' / 'notepad.exe'.
	self 
		assert: 'file:///C:' asUrl asFileReference
		equals: FileLocator C
]

{ #category : #testing }
ZnFileUrlTests >> testWrongScheme [
	self should: [ 'http://zn.stfx.eu/zn/small.html' asZnUrl asFileReference ] raise: AssertionFailure
]
