public
openFileStream: path writable: writable
	| fullPath |
	fullPath := self stringFromPath: path.
	"redirect over the default implementation"
	^ writable 
		ifFalse: [ FileStream readOnlyFileNamed: fullPath ]
		ifTrue: [ FileStream fileNamed: fullPath ]