public
isFile: aPath
	| entry |
	
	aPath isRoot ifTrue: [ ^ false ].
	
	entry := self  basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: [  false ]
		ifFalse: [ self basicIsFile: entry ].
	