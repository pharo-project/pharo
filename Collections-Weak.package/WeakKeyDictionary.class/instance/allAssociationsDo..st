enumerating
allAssociationsDo: aBlock 
	"Evaluate aBlock for each of the receiver's elements (key/value 
	associations). This includes associations with nil keys, except for
	the expired ones."

	super associationsDo: [:association | 
		association expired ifFalse:[aBlock value: association]].