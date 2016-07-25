adding
addAll: aCollection 
 	"Include all the elements of aCollection as the receiver's elements"
 
 	(aCollection respondsTo: #associationsDo:)
 		ifTrue: [aCollection associationsDo: [:ass | self add: ass]]
 		ifFalse: [aCollection do: [:each | self add: each]].
 	^ aCollection