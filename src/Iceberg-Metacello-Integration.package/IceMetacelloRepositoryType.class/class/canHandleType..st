testing
canHandleType: aType 
	^ self allTypes
		anySatisfy: [ :each | each isSuitableForType: aType ]  
	