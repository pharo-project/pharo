accessing
object: anObject
	Smalltalk garbageCollect.
	super object: (EyePointerWrapper wrap: anObject)