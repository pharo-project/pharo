"
A class trait definition
"
Class {
	#name : #MCClassTraitDefinition,
	#superclass : #MCDefinition,
	#instVars : [
		'baseTrait',
		'classTraitComposition',
		'category'
	],
	#category : #'Monticello-Modeling'
}

{ #category : #'instance creation' }
MCClassTraitDefinition class >> baseTraitName: aString classTraitComposition: classTraitCompositionString [
	^ self baseTraitName: aString classTraitComposition: classTraitCompositionString category: nil
]

{ #category : #'instance creation' }
MCClassTraitDefinition class >> baseTraitName: aString classTraitComposition: classTraitCompositionString category: aCategoryString [
	^ self new
		initializeWithBaseTraitName: aString
		classTraitComposition: classTraitCompositionString
		category: aCategoryString
]

{ #category : #comparing }
MCClassTraitDefinition >> = aDefinition [
	^ super = aDefinition
		and: [ baseTrait = aDefinition baseTrait
		and: [ self classTraitCompositionString = aDefinition classTraitCompositionString
		and: [ category = aDefinition category ] ] ]
]

{ #category : #visiting }
MCClassTraitDefinition >> accept: aVisitor [
	^ aVisitor visitClassTraitDefinition: self
]

{ #category : #accessing }
MCClassTraitDefinition >> baseTrait [
	^baseTrait

]

{ #category : #accessing }
MCClassTraitDefinition >> category [
	^ category ifNil: [
		(Smalltalk classOrTraitNamed: self baseTrait)
			ifNotNil: [ :bTrait | bTrait category ]
			ifNil: [ self error: 'Can''t detect the category' ] ]
]

{ #category : #accessing }
MCClassTraitDefinition >> className [
	^self baseTrait
]

{ #category : #accessing }
MCClassTraitDefinition >> classTraitComposition [
	^classTraitComposition


]

{ #category : #accessing }
MCClassTraitDefinition >> classTraitCompositionString [
	^ self classTraitComposition ifNil: [ '{}' ]
]

{ #category : #accessing }
MCClassTraitDefinition >> definitionString [
	^self baseTrait , ' classTrait
	uses: ' , self classTraitCompositionString.

]

{ #category : #accessing }
MCClassTraitDefinition >> description [
	^ Array with: baseTrait with: classTraitComposition
]

{ #category : #comparing }
MCClassTraitDefinition >> hash [
	| hash |
	hash := String stringHash: baseTrait initialHash: 0.
	hash := String stringHash: self classTraitCompositionString initialHash: hash.
	category ifNotNil: [ :cat | hash := String stringHash: cat initialHash: hash ].
	^ hash
]

{ #category : #initialization }
MCClassTraitDefinition >> initializeWithBaseTraitName: aTraitName classTraitComposition: aString [
	baseTrait := aTraitName.
	classTraitComposition := aString
]

{ #category : #initialization }
MCClassTraitDefinition >> initializeWithBaseTraitName: aTraitName classTraitComposition: aString category: aCategoryString [
	baseTrait := aTraitName.
	classTraitComposition := aString.
	category := aCategoryString
]

{ #category : #testing }
MCClassTraitDefinition >> isClassDefinition [
	"Traits are treated the same like classes."
	
	^ true
]

{ #category : #installing }
MCClassTraitDefinition >> load [	
	self class compiler evaluate: self definitionString
]

{ #category : #printing }
MCClassTraitDefinition >> printDefinitionOn: stream [
	stream
		nextPutAll: self baseTrait;
		nextPutAll: ' classTrait';
		cr;
		tab;
		nextPutAll: 'uses: ';
		nextPutAll: self classTraitCompositionString
]

{ #category : #accessing }
MCClassTraitDefinition >> requirements [
	^Array with: baseTrait
]

{ #category : #accessing }
MCClassTraitDefinition >> sortKey [
	^ self baseTrait , '.classTrait'
]

{ #category : #accessing }
MCClassTraitDefinition >> source [
	^self definitionString
]

{ #category : #printing }
MCClassTraitDefinition >> summary [
	^self baseTrait , ' classTrait'

]
