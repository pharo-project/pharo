printing
printTemporaries: tempSequence on: aStream doPrior: aBlock
	"Print any in-scope temporaries.  If there are any evaluate aBlock
	 prior to printing.  Answer whether any temporaries were printed."
	| tempStream seen |
	tempSequence ifNil:
		[^false].
	tempStream := (String new: 16) writeStream.
	"This is for the decompiler which canmot work out which optimized block a particular temp is
	 local to and hence may produce diplicates as in
		expr ifTrue: [| aTemp | ...] ifFalse: [| aTemp | ...]"
	seen := Set new.
	tempSequence do:
		[:tempNode |
		tempNode isIndirectTempVector
			ifTrue:
				[tempNode remoteTemps do:
					[:tempVariableNode|
					 (tempVariableNode scope >= 0
					  and: [(seen includes: tempNode key) not]) ifTrue:
						[tempStream space; nextPutAll: (seen add: tempVariableNode key)]]]
			ifFalse:
				[(tempNode scope >= -1
				  and: ["This is for the decompiler which may create a block arg when converting
						a while into a to:do: but won't remove it form temporaries"
					   tempNode isBlockArg not
				  and: [(seen includes: tempNode key) not]]) ifTrue:
					[tempStream space; nextPutAll: (seen add: tempNode key)]]].
	tempStream position = 0 ifTrue:
		[^false].
	aBlock value.
	aStream nextPut: $|; nextPutAll: tempStream contents; space; nextPut: $|.
	^true