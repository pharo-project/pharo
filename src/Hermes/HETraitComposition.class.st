Class {
	#name : #HETraitComposition,
	#superclass : #HEArray,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HETraitComposition class >> tag [
	^18
]

{ #category : #converting }
HETraitComposition >> asLiteralIn: env [
	^ TraitComposition new
		transformations: (value collect: [ :e | e asLiteralIn: env ]) asOrderedCollection;
		yourself
]

{ #category : #accessing }
HETraitComposition >> value: aTraitComposition [
	value := aTraitComposition transformations asArray collect:#asExportedLiteral.
]
