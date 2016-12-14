running
classInstanceVariableIndexAt: anIndex
	"number of instances variables of the class Class. Classes that have more than this number in their fixed size have their own instance class variable that we should probably take care of."
	^ (self classNamed: #Class) mirror fixedSize + anIndex