running
createFalse

	"False format should be changed and calculated depending on the image"
	| falseClassIndex falseObject newOop |
	falseClassIndex := self nextOrdinaryClassIndex.
	falseObject := self objectSpace backend 
		createObjectWithFormat: False format withSize: 0 classIndex: falseClassIndex.
	objectSpace falseObject: falseObject.
	^ objectSpace falseObject
