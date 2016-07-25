printing
printOn: aStream
	
	self transformations isEmptyOrNil
		ifFalse: [
			
			"The first element does not need parenthesis"
			aStream nextPutAll: (self transformations at: 1) asString.
			(self transformations size > 1) ifTrue: [ aStream nextPutAll: ' + '].
			
			"The rest of the elements either need or not parenthesis depending if they are trait compositions or traits"
			(self transformations allButFirst)
				do: [:each | aStream nextPutAll: (each isTrait 
														ifTrue: [ each asString]
														ifFalse: [ '(', each asString, ')' ])]										 
				separatedBy: [aStream nextPutAll: ' + '] ]
			
		ifTrue: [aStream nextPutAll: '{}']