Extension { #name : #TClass }

{ #category : #'*Ring-Core-Kernel' }
TClass >> asFullRingDefinition [
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
]

{ #category : #'*Ring-Core-Kernel' }
TClass >> asRingDefinition [
	^self explicitRequirement.
]

{ #category : #'*Ring-Core-Kernel' }
TClass >> asRingDefinitionWithMethods: methodsBoolean withSuperclasses: supersBoolean withSubclasses: subsBoolean withPackageKeys: packageKeys in: aRGSlice [
	^self explicitRequirement.
]

{ #category : #'*Ring-Core-Kernel' }
TClass >> asRingDefinitionWithMethods: methodsBoolean withSuperclasses: supersBoolean withSubclasses: subsBoolean withPackages: packsBoolean [
	"Retrieves a ring class/trait based on the receiver.
	The data loaded in the class/trait (active methods, superclasses, subclasses and packages) is requested by the users.
	As it may need to traverse hierarchies for retrieving super and subclasses a ring slice is created as the container for every class, method and package.
	To retrieve the slice:  aRGClass environment "
	| rgClass rgSlice  rgPackageKeys |
	
	rgSlice := RGSlice named: #fromImage.
	packsBoolean ifTrue: [ 
		rgPackageKeys := rgSlice loadPackagesFromImage ].
	rgClass := self asRingDefinitionWithMethods: methodsBoolean withSuperclasses: supersBoolean withSubclasses: subsBoolean withPackageKeys: rgPackageKeys in: rgSlice.
	rgSlice cleanEmptyPackages.
	rgSlice loadTraitUsers.
		
	^ rgClass
]
