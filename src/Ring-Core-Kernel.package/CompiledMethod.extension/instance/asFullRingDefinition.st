*Ring-Core-Kernel
asFullRingDefinition

	"Retrieves an active RGMethodDefinition which knows its parent <class>.
	Note that the full conversion does not happen at the level of the class. If you need that request asFullRingDefinition to the class"
	| rgClass rgMethod |
	rgClass := self realClass asRingDefinition.
	rgMethod := self asActiveRingDefinition.
	rgClass addMethod: rgMethod.
	rgMethod package: (RGContainer packageOfMethod: rgMethod).
	^ rgMethod