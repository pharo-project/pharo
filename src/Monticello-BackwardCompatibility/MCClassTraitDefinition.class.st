"
This class is just here for backward compatibility and should not be used anymore. It is just here to load old code.
"
Class {
	#name : 'MCClassTraitDefinition',
	#superclass : 'MCDefinition',
	#instVars : [
		'baseTrait',
		'classTraitComposition',
		'packageName',
		'tagName'
	],
	#category : 'Monticello-BackwardCompatibility',
	#package : 'Monticello-BackwardCompatibility'
}

{ #category : 'instance creation' }
MCClassTraitDefinition class >> baseTraitName: aString [

	^ self new
		  baseTraitName: aString;
		  yourself
]

{ #category : 'comparing' }
MCClassTraitDefinition >> = aDefinition [
	^ super = aDefinition
		and: [ baseTrait = aDefinition baseTrait
		and: [ self classTraitCompositionString = aDefinition classTraitCompositionString
		and: [ self packageName = aDefinition packageName
		and: [  self tagName = aDefinition tagName] ] ] ]
]

{ #category : 'visiting' }
MCClassTraitDefinition >> accept: aVisitor [
	^ aVisitor visitClassTraitDefinition: self
]

{ #category : 'accessing' }
MCClassTraitDefinition >> baseTrait [
	^baseTrait

]

{ #category : 'initialization' }
MCClassTraitDefinition >> baseTraitName: aTraitName [

	baseTrait := aTraitName
]

{ #category : 'accessing' }
MCClassTraitDefinition >> category [

	^ self packageName
		  ifNil: [
			  (Smalltalk classOrTraitNamed: self baseTrait)
				  ifNotNil: [ :bTrait | bTrait category ]
				  ifNil: [ self error: 'Can''t detect the category' ] ]
		  ifNotNil: [ :package |
			  self tagName
				  ifNil: [ package ]
				  ifNotNil: [ :tag | package , '-' , tag ] ]
]

{ #category : 'initialization' }
MCClassTraitDefinition >> category: aCategoryString [

	self packageName: aCategoryString
]

{ #category : 'accessing' }
MCClassTraitDefinition >> classDefinitionString [

	^self definitionString
]

{ #category : 'accessing' }
MCClassTraitDefinition >> className [
	^self baseTrait
]

{ #category : 'accessing' }
MCClassTraitDefinition >> classTraitComposition [
	^classTraitComposition


]

{ #category : 'initialization' }
MCClassTraitDefinition >> classTraitComposition: aString [

	classTraitComposition := aString
]

{ #category : 'accessing' }
MCClassTraitDefinition >> classTraitCompositionString [
	^ self classTraitComposition ifNil: [ '{}' ]
]

{ #category : 'accessing' }
MCClassTraitDefinition >> definitionString [
	^self baseTrait , ' classTrait
	uses: ' , self classTraitCompositionString.

]

{ #category : 'accessing' }
MCClassTraitDefinition >> description [
	^ Array with: baseTrait with: classTraitComposition
]

{ #category : 'comparing' }
MCClassTraitDefinition >> hash [

	| hash |
	hash := String stringHash: baseTrait initialHash: 0.
	hash := String stringHash: self classTraitCompositionString initialHash: hash.
	self packageName ifNotNil: [ :string | hash := String stringHash: string initialHash: hash ].
	self tagName ifNotNil: [ :straing | hash := String stringHash: straing initialHash: hash ].
	^ hash
]

{ #category : 'testing' }
MCClassTraitDefinition >> isClassDefinition [
	"Traits are treated the same like classes."
	
	^ true
]

{ #category : 'testing' }
MCClassTraitDefinition >> isTraitDefinition [
	^ true
]

{ #category : 'installing' }
MCClassTraitDefinition >> load [	
	self class compiler evaluate: self definitionString
]

{ #category : 'accessing' }
MCClassTraitDefinition >> packageName [

	^ packageName
]

{ #category : 'accessing' }
MCClassTraitDefinition >> packageName: anObject [

	packageName := anObject
]

{ #category : 'printing' }
MCClassTraitDefinition >> printDefinitionOn: stream [
	stream
		nextPutAll: self baseTrait;
		nextPutAll: ' classTrait';
		cr;
		tab;
		nextPutAll: 'uses: ';
		nextPutAll: self classTraitCompositionString
]

{ #category : 'accessing' }
MCClassTraitDefinition >> requirements [
	^ { baseTrait }
]

{ #category : 'accessing' }
MCClassTraitDefinition >> sortKey [
	^ self baseTrait , '.classTrait'
]

{ #category : 'accessing' }
MCClassTraitDefinition >> source [
	^self definitionString
]

{ #category : 'printing' }
MCClassTraitDefinition >> summary [
	^self baseTrait , ' classTrait'

]

{ #category : 'accessing' }
MCClassTraitDefinition >> tagName [

	^ tagName
]

{ #category : 'accessing' }
MCClassTraitDefinition >> tagName: aString [

	tagName := aString = Package rootTagName
		           ifTrue: [ nil ]
		           ifFalse: [ aString asSymbol ]
]
