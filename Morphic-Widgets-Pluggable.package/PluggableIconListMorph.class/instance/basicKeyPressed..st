event
basicKeyPressed: aChar 
	
	^ aChar == Character space
		ifTrue: [ self interactWithSelectedItem ]
		ifFalse: [ super basicKeyPressed: aChar ]