Class {
	#name : #HETraitExclusion,
	#superclass : #HETraitTransformation,
	#category : 'Hermes-Model'
}

{ #category : #accessing }
HETraitExclusion class >> tag [
	^ 21.
]

{ #category : #converting }
HETraitExclusion >> asLiteralIn: env [
	^ TraitExclusion new
		exclusions: (value collect: [ :e | e asLiteralIn: env ]);
		subject: (subject asLiteralIn: env);
		yourself
]

{ #category : #accessing }
HETraitExclusion >> value: aTraitExclusion [
	super value: aTraitExclusion.
	value := aTraitExclusion exclusions collect:#asExportedLiteral.
]
