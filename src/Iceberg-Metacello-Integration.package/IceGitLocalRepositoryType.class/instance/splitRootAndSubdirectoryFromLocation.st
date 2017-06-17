private
splitRootAndSubdirectoryFromLocation
	| root subDir |
	
	(self location beginsWith: (self class type, '://'))	
		ifFalse: [ self error: 'Invalid URL (It should be ', self class type, '://...)' ].
	root := (self location allButFirst: self class type size + 3) asFileReference.
	subDir := #().
	[ root isNil or: [ self isGitRoot: root ] ] 
		whileFalse: [ 
			subDir := subDir copyWithFirst: root basename.
			root := root parent ].
	root ifNil: [ self error: 'I can''t find a .git/config file.' ].
	
	^ { 
		root. "a FileReference"
		String streamContents: [ :stream | subDir asStringOn: stream delimiter: $/ ] "a String" }