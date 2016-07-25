private
handleCircularReferencesWhile: aBlock
	"Used to handle circular references as explained in the class comment"

	lock ifTrue: [ ^ self ].
	lock := true.
	
	aBlock ensure: [ lock := false ].