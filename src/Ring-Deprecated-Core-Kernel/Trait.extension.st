Extension { #name : #Trait }

{ #category : #'*Ring-Deprecated-Core-Kernel' }
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

{ #category : #'*Ring-Deprecated-Core-Kernel' }
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
