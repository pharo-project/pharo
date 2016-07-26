removing
removeAllSuchThat: aBlock 
	"Evaluate aBlock for each element and remove all that elements from
	the receiver for that aBlock evaluates to true.  For LinkedLists, it's safe to use do:."

	self do: [:each | (aBlock value: each) ifTrue: [self remove: each]]