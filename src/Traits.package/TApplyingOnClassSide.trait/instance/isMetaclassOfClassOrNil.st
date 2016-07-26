class hierarchy
isMetaclassOfClassOrNil
	
	
	^ self theNonMetaClass 
		ifNil: [ true ] 
		ifNotNil: [ :nonMetaClass | nonMetaClass == Class ] 