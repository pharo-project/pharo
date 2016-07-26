method accessing
compile: aString 
	^ self compile: aString withAttributesFrom: (self methodFor: (RBParser parseMethodPattern: aString))