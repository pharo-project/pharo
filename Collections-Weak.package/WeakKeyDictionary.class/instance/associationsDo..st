enumerating
associationsDo: aBlock 
	"Evaluate aBlock for each of the receiver's elements (key/value 
	associations)."

	

	super associationsDo: [:association | | key | 
		"Hold onto the key so it won't be collected while the block is evaluated."
		key := association key.
		key ifNotNil:[aBlock value: association]].