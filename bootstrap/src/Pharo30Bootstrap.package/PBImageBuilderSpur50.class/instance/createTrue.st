running
createTrue

	"True format should be changed and calculated depending on the image"
	| trueClassIndex trueObject newOop |
	trueClassIndex := self nextOrdinaryClassIndex.
	trueObject := self objectSpace backend 
		createObjectWithFormat: True format withSize: 0 classIndex: trueClassIndex.
	objectSpace trueObject: trueObject.
	^ objectSpace trueObject
