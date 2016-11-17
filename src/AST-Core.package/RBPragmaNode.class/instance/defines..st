testing
defines: aName
	^ self isPrimitive and: [ arguments anySatisfy: [ :each | each value = aName ] ]