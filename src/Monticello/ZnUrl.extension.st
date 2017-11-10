Extension { #name : #ZnUrl }

{ #category : #'*Monticello' }
ZnUrl >> asFileReferenceWithRelativePath [
	"Convert the receiver into a new FileReference object accepting relative paths.
	This works for filetree (a special pharo format).
	Note that for a FileReference a trailing slash is not relevant"
	| path |
	
	self 
		assert: self scheme = #filetree 
		description: 'Only filetree:// URL can be converted to a FileReference with relative Paths'.
	
	self isSlash ifTrue: [ ^ FileSystem root ].
	
	path := self host 
		ifNotNil: [ segments copyWithFirst: self host ]
		ifNil: [ segments copyWithFirst: FileSystem disk delimiter asString ].
		
	^ (String streamContents: [ :stream |
		path 
			asStringOn: stream 
			delimiter: FileSystem disk delimiter asString ])
		asFileReference
]

{ #category : #'*Monticello' }
ZnUrl >> location [
	"Returns the URL location string without username and password"
	^ String streamContents: [ :stream |
		self hasScheme ifTrue: [ 
			stream nextPutAll: self scheme; nextPut: $:.
		self isSchemeUsingDoubleSlash ifTrue: [ stream nextPut: $/; nextPut: $/ ] ].
		self hasHost ifTrue: [ self encode: self host on: stream ].
		self hasPort ifTrue: [ stream nextPut: $:; print: self port ].
		self printPathQueryFragmentOn: stream ]
]
