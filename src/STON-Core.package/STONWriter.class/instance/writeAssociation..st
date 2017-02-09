writing
writeAssociation: association
	jsonMode
		ifTrue: [ self error: 'wrong object class for JSON mode' ].
	self 
		encodeKey: association key 
		value: association value