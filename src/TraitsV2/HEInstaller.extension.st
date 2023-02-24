Extension { #name : #HEInstaller }

{ #category : #'*TraitsV2' }
HEInstaller >> buildTrait: aTraitDefinition [
	| newTrait traitComposition traitClass|

	(self existingTrait: aTraitDefinition) ifNotNil: [:x | ^ x ].

	traitClass := Smalltalk globals at: #Trait ifAbsent: [ self error: 'Trait support is not installed' ].

	traitComposition := self buildTraitCompositionFor: aTraitDefinition traitComposition.

	newTrait := traitClass
		named: aTraitDefinition traitName
		uses: traitComposition
		package: aTraitDefinition category
		env: environment.

	newTrait classTrait traitComposition: (self buildTraitCompositionFor: aTraitDefinition classTraitComposition).

	^ newTrait
]

{ #category : #'*TraitsV2' }
HEInstaller >> buildTraitCompositionFor: traitComposition [

	| aLiteral |

	aLiteral := (traitComposition asLiteralIn: environment).
	^ aLiteral isArray
		ifTrue: [ aLiteral
							ifEmpty: [ TaEmptyComposition new ]
							ifNotEmpty: [ TaSequence withAll: (aLiteral collect: [:each | each asTraitComposition]) ]  ]
		ifFalse: [ aLiteral asTraitComposition ]
]
