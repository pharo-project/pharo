operations
renameTo: newBasename
	
	| result |
	result := self resolve renameTo: newBasename.
	path size > 0
		ifTrue: [ path basename: newBasename ]
		ifFalse: [ path := result path ]