checking
checkBasicClasses
	"Check that no indexes of instance vars have changed in certain classes.  If you get an error in this method, an implementation of veryDeepCopyWith: needs to be updated.  The idea is to catch a change while it is still in the system of the programmer who made it.  
	DeepCopier new checkAllClasses"
	Object instSize = 0 ifFalse: [ self error: 'Many implementers of veryDeepCopyWith: are out of date' ].


