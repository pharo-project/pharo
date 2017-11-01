Extension { #name : #Trait }

{ #category : #'*Ring-Core-Kernel' }
Trait >> asFullRingDefinition [
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
Trait >> asRingDefinition [
	"A triat is converted to a ring class. Only the receiver is converted.
	Methods, superclasses, subclasses are not generated"

	| ring |
	ring:= (RGTraitDefinition named: self name)
		category: self category;
		superclassName: #Trait;  
		traitCompositionSource: self traitCompositionString;
		comment: self organization classComment;
		stamp: self organization commentStamp;
		definitionSource: self definition;
		withMetaclass.
		
	ring theMetaClass 
		traitCompositionSource: self theMetaClass traitCompositionString;
		definitionSource: self theMetaClass definition.
	^ring
]

{ #category : #'*Ring-Core-Kernel' }
Trait >> asRingDefinitionWithMethods: methodsBoolean withSuperclasses: supersBoolean withSubclasses: subsBoolean withPackageKeys: packageKeys in: aRGSlice [
	
	| rgClass rgMethod rgSuper sup |	
	rgClass := self asRingDefinition.
	aRGSlice loadClass: rgClass using: packageKeys.

	methodsBoolean ifTrue: [
		self methodsDo:[ :mth|  
			rgMethod := mth asActiveRingDefinition.
			aRGSlice loadMethod: rgMethod inClass: rgClass using: packageKeys ].
		self theMetaClass methodsDo:[ :mth|  
			rgMethod := mth asActiveRingDefinition.
			aRGSlice loadMethod: rgMethod inClass: rgClass theMetaClass using: packageKeys ] ].

	supersBoolean ifTrue: [
		rgSuper := aRGSlice classNamed: rgClass superclassName.
		rgSuper isNil ifTrue: [
			sup := aRGSlice environment classNamed: rgClass superclassName.
			rgSuper := sup asRingDefinitionWithMethods: methodsBoolean withSuperclasses: supersBoolean withSubclasses: subsBoolean withPackageKeys: packageKeys in: aRGSlice ].
		rgClass superclass: rgSuper ].
	
	^rgClass
]

{ #category : #'*Ring-Core-Kernel' }
Trait >> asRingDefinitionWithMethods: methodsBoolean withSuperclasses: supersBoolean withSubclasses: subsBoolean withPackages: packsBoolean [
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
