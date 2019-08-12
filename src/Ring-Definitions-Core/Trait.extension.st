Extension { #name : #Trait }

{ #category : #'*Ring-Definitions-Core' }
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
		definitionSource: self oldDefinition;
		withMetaclass.
		
	ring classSide 
		traitCompositionSource: self classSide traitCompositionString;
		definitionSource: self classSide oldDefinition.

	^ring
]

{ #category : #'*Ring-Definitions-Core' }
Trait >> asRingDefinitionWithMethods: methodsBoolean withSuperclasses: supersBoolean withSubclasses: subsBoolean withPackageKeys: packageKeys in: aRGSlice [

	| rgClass rgMethod rgSuper sup |

	rgClass := self asRingDefinition.
	aRGSlice loadClass: rgClass using: packageKeys.

	methodsBoolean
		ifTrue: [ self
				methodsDo: [ :mth | 
					rgMethod := mth asActiveRingDefinition.
					aRGSlice loadMethod: rgMethod inClass: rgClass using: packageKeys
					].
			self classSide
				methodsDo: [ :mth | 
					rgMethod := mth asActiveRingDefinition.
					aRGSlice loadMethod: rgMethod inClass: rgClass classSide using: packageKeys
					]
			].

	supersBoolean
		ifTrue: [ rgSuper := aRGSlice classNamed: rgClass superclassName.
			rgSuper
				ifNil: [ sup := aRGSlice environment classNamed: rgClass superclassName.
					rgSuper := sup
						asRingDefinitionWithMethods: methodsBoolean
						withSuperclasses: supersBoolean
						withSubclasses: subsBoolean
						withPackageKeys: packageKeys
						in: aRGSlice
					].
			rgClass superclass: rgSuper
			].

	^ rgClass
]
