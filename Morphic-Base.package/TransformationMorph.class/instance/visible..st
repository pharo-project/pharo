geometry etoy
visible: aBoolean
	"Set the receiver's visibility property"

	super visible: aBoolean.
	submorphs isEmptyOrNil ifFalse: [submorphs first visible: aBoolean]