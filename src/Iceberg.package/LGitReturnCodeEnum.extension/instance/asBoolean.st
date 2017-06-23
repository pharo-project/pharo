*Iceberg-Core
asBoolean
	^ self value = 0 ifTrue: false ifFalse: 
		[ self value = 1 ifTrue: true ifFalse: 
		[ self error: self printString, ' can''t be converted to a boolean' ]]