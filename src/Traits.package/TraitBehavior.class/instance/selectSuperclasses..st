enumerating
selectSuperclasses: aBlock 
	"Evaluate the argument, aBlock, with the receiver's superclasses as the 
	argument. Collect into an OrderedCollection only those superclasses for 
	which aBlock evaluates to true. In addition, evaluate aBlock for the 
	superclasses of each of these successful superclasses and collect into the 
	OrderedCollection ones for which aBlock evaluates to true. Answer the 
	resulting OrderedCollection."

	| aSet |
	aSet := Set new.
	self allSuperclasses do: 
		[:aSuperclass | 
		(aBlock value: aSuperclass) ifTrue: [aSet add: aSuperclass]].
	^aSet