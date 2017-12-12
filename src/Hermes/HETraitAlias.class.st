Class {
	#name : #HETraitAlias,
	#superclass : #HETraitTransformation,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HETraitAlias class >> tag [
	^ 20.
]

{ #category : #converting }
HETraitAlias >> asLiteralIn: env [
	^ TraitAlias new
		aliases: (value collect: [ :e | e asLiteralIn: env ]);
		subject: (subject asLiteralIn: env); 
		yourself
]

{ #category : #accessing }
HETraitAlias >> value: aTraitAlias [
	super value:aTraitAlias.
	value := aTraitAlias aliases collect:#asExportedLiteral.
	
]
