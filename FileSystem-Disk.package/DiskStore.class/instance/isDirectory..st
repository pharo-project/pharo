public
isDirectory: aPath
	| entry |
	
	aPath isRoot ifTrue: [ ^ true ].
	
	entry := self  basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: [  false ]
		ifFalse: [ self basicIsDirectory: entry ].
	