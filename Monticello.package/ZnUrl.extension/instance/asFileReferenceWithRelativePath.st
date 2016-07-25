*Monticello
asFileReferenceWithRelativePath
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