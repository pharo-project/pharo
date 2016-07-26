*Ring-Core-Kernel
asFullRingDefinition
	"A behavior is converted to a ring class including its variables, methods, direct superclass, direct subclasses and the package in which is loaded.
	Active methods are generated and each knows its package as well.
	Note that for its direct superclass and subclasses no full definitions are requested. If you need to traverse hierarchies use #asRingDefinitionWithMethods:withSuperclasses:withSubclasses:withPackages:"
	
	| rgClass rgSuper rgSub rgMethod packageKeys |
	rgClass:= self asRingDefinition.
	rgClass package: (RGContainer packageOfClass: rgClass).
	
	self superclass notNil 
		ifTrue: [ 
			rgSuper := self superclass asRingDefinition.
			rgClass superclass: rgSuper ]
		ifFalse: [  
			self isTrait ifTrue: [ 
				rgSuper := Trait asRingDefinition.
				rgClass superclass: rgSuper. ]
			].

	self subclasses do:[ :each |
		rgSub := each asRingDefinition.
		rgSub superclass: rgClass ].
	
	packageKeys := RGContainer packageKeys.
	self methodsDo:[ :mth|  
			rgMethod := mth asActiveRingDefinition.
			rgClass addMethod: rgMethod.
			rgMethod package: (RGContainer packageOfMethod: rgMethod using: packageKeys) ].
	self theMetaClass methodsDo:[ :mth|  
			rgMethod := mth asActiveRingDefinition.
			rgClass theMetaClass addMethod: rgMethod.
			rgMethod package: (RGContainer packageOfMethod: rgMethod using: packageKeys) ].
	
	^ rgClass