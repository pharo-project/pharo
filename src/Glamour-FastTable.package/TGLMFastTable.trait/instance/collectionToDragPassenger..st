private
collectionToDragPassenger: aCollection
	^ aCollection size = 1
		ifTrue: [ aCollection anyOne ] 
		ifFalse: [ aCollection ]