"
I extend the ShiftClassBuilder to have the old implementation of traits
"
Class {
	#name : #ShOldTraitsBuilderEnhancer,
	#superclass : #ShDefaultBuilderEnhancer,
	#category : 'Shift-ClassBuilder'
}

{ #category : #'class modifications' }
ShOldTraitsBuilderEnhancer >> afterMethodsCompiled: aBuilder [
	super afterMethodsCompiled: aBuilder.
	
	self
			installTraitComposition: (self traitCompositionOf: aBuilder)
			on: aBuilder newClass
			from: aBuilder oldClass.

	self
			installTraitComposition: (self classTraitCompositionOf: aBuilder)
			on: aBuilder newMetaclass
			from: aBuilder oldMetaclass.
]

{ #category : #migrating }
ShOldTraitsBuilderEnhancer >> afterMigratingClass: aBuilder installer: anInstaller [
	aBuilder newClass traitComposition traits do: [:each | each addUser: aBuilder newClass].
	aBuilder newMetaclass traitComposition traits do: [:each | each addUser: aBuilder newMetaclass]
]

{ #category : #migrating }
ShOldTraitsBuilderEnhancer >> beforeMigratingClass: aBuilder installer: anInstaller [
	aBuilder oldClass ifNotNil:[ :x | x traitComposition traits do: [:each | each removeUser: x]].	
	aBuilder oldMetaclass ifNotNil:[ :x | x traitComposition traits do: [:each | each removeUser: x]]

]

{ #category : #accessing }
ShOldTraitsBuilderEnhancer >> classTraitCompositionOf: aBuilder [ 
	^ aBuilder classTraitComposition
		ifNil: [ (aBuilder buildEnvironment hasClassNamed: aBuilder name)
				ifTrue: [ (aBuilder buildEnvironment classNamed: aBuilder name) class traitComposition ]
				ifFalse: [ '' asTraitComposition ] ]
]

{ #category : #initialization }
ShOldTraitsBuilderEnhancer >> initializeBuilder: aBuilder [
	super initializeBuilder: aBuilder.
	aBuilder propertyAt: #traitComposition put: nil.
	aBuilder propertyAt: #classTraitComposition put: nil	.
	
	aBuilder addChangeComparer: ShTraitCompositionChangedDetector.
	aBuilder addChangeComparer: ShClassTraitCompositionChangedDetector.	
]

{ #category : #building }
ShOldTraitsBuilderEnhancer >> installTraitComposition: anObject on: aClass from: anOldClass [
	| aTraitComposition |
	aTraitComposition := anObject asTraitComposition.

	aTraitComposition = #() asTraitComposition ifTrue:[^self].

	aTraitComposition assertValidUser: aClass.
	aClass traitComposition: aTraitComposition.

	aTraitComposition allSelectors do:[:e | aClass updateMethodDictionarySelector: e].
]

{ #category : #accessing }
ShOldTraitsBuilderEnhancer >> traitCompositionOf: aBuilder [
	^ aBuilder traitComposition
		ifNil: [ (aBuilder buildEnvironment hasClassNamed: aBuilder name)
				ifTrue: [ (aBuilder buildEnvironment classNamed: aBuilder name) traitComposition ]
				ifFalse: [ '' asTraitComposition ] ]
]
