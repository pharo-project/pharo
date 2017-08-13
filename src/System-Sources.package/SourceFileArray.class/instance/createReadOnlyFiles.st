private
createReadOnlyFiles

	| readOnly failed |
	
	failed := false.
	readOnly := files collect: [ :file | 
		file 
			ifNil: [  failed := true. nil ]
			ifNotNil: [ [ file readOnlyCopy ] 
        		on: FileDoesNotExistException
        		do: [ failed := true. nil ] ]
	].

	^ failed ifFalse: [ readOnly ] ifTrue: [ self closeFileArray: readOnly. nil ]