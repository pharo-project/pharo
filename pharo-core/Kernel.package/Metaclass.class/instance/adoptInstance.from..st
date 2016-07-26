initialize-release
adoptInstance: oldInstance from: oldMetaClass 
	"Recreate any existing instances of the argument, oldClass, as instances of 
	the receiver, which is a newly changed class. Permute variables as 
	necessary."
	thisClass class == self ifTrue:[^self error:'Metaclasses have only one instance'].
	oldMetaClass isMeta ifFalse:[^self error:'Argument must be Metaclass'].
	oldInstance class == oldMetaClass ifFalse:[^self error:'Not the class of argument'].
	^thisClass := self 
		newInstanceFrom: oldInstance 
		variable: self isVariable 
		size: self instSize 
		map: (self instVarMappingFrom: oldMetaClass)