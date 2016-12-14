running
createNil

	"UndefinedObject format should be changed and calculated depending on the image"
	| undefinedObjectClassIndex nilObject newOop |
	undefinedObjectClassIndex := self nextOrdinaryClassIndex.
	nilObject := self objectSpace backend 
		createObjectWithFormat: UndefinedObject format withSize: 0 classIndex: undefinedObjectClassIndex.
	objectSpace nilObject: nilObject.
	^ objectSpace nilObject