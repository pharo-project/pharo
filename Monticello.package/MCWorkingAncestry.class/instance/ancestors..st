accessing
ancestors: anObject
	ancestors := anObject collect: [ :a | a lazy ]