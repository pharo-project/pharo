Extension { #name : #Class }

{ #category : #'*Ring-Definitions-Core' }
Class >> asFullRingDefinition [
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
	self classSide methodsDo:[ :mth|  
			rgMethod := mth asActiveRingDefinition.
			rgClass classSide addMethod: rgMethod.
			rgMethod package: (RGContainer packageOfMethod: rgMethod using: packageKeys) ].
	
	^ rgClass
]

{ #category : #'*Ring-Definitions-Core' }
Class >> asRingDefinition [
	"A behavior is converted to a ring class. Only the receiver and its variables (instance, class, pools) are converted.
	Methods, superclasses, subclasses are not generated"
	
	| ring |
	ring := (RGClassDefinition named: self name)
		category: self category;
		superclassName: (self superclass
			ifNil: [ nil printString ]
			ifNotNil: [ self superclass name ]);
		traitCompositionSource: self traitCompositionString;
		addInstanceVariables: self instVarNames;
		addClassVariables: self classVarNames;
		addSharedPools: self sharedPoolNames;
		comment: self organization classComment;
		stamp: self organization commentStamp;
		definitionSource: self oldDefinition;
		package: self package asRingDefinition;
		withMetaclass.
	ring classSide 
		traitCompositionSource: self classSide traitCompositionString;
		definitionSource: self classSide oldDefinition;
		addInstanceVariables: self classSide instVarNames.  
	^ ring
]

{ #category : #'*Ring-Definitions-Core' }
Class >> asRingDefinitionWithMethods: methodsBoolean withSuperclasses: supersBoolean withSubclasses: subsBoolean withPackageKeys: packageKeys in: aRGSlice [
	| rgClass rgMethod rgSuper rgSub subs |
	rgClass := self asRingDefinition.
	aRGSlice loadClass: rgClass using: packageKeys.
	methodsBoolean
		ifTrue: [ 
			self
				methodsDo: [ :mth | 
					rgMethod := mth asActiveRingDefinition.
					aRGSlice loadMethod: rgMethod inClass: rgClass using: packageKeys ].
			self classSide
				methodsDo: [ :mth | 
					rgMethod := mth asActiveRingDefinition.
					aRGSlice loadMethod: rgMethod inClass: rgClass classSide using: packageKeys ] ].
	supersBoolean
		ifTrue: [ 
			self superclass
				ifNotNil: [ 
					rgSuper := aRGSlice classNamed: self superclass name.
					rgSuper
						ifNil: [ 
							rgSuper := self superclass
								asRingDefinitionWithMethods: methodsBoolean
								withSuperclasses: supersBoolean
								withSubclasses: subsBoolean
								withPackageKeys: packageKeys
								in: aRGSlice ].
					rgClass superclass: rgSuper ] ].
	subsBoolean
		ifTrue: [ 
			subs := self subclasses reject: [ :sub | sub isMeta ].
			rgClass name = #Trait
				ifTrue: [ subs := aRGSlice environment allTraits ].
			subs
				do: [ :each | 
					rgSub := aRGSlice classNamed: each name.
					rgSub
						ifNil: [ 
							rgSub := each
								asRingDefinitionWithMethods: methodsBoolean
								withSuperclasses: supersBoolean
								withSubclasses: subsBoolean
								withPackageKeys: packageKeys
								in: aRGSlice ].
					rgSub superclass: rgClass ] ].
	^ rgClass
]

{ #category : #'*Ring-Definitions-Core' }
Class >> asRingDefinitionWithMethods: methodsBoolean withSuperclasses: supersBoolean withSubclasses: subsBoolean withPackages: packsBoolean [
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
