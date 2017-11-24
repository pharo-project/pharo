"
A MCTraitDefinition represents a trait.
"
Class {
	#name : #MCTraitDefinition,
	#superclass : #MCClassDefinition,
	#category : #'Monticello-Modeling'
}

{ #category : #'instance-creation' }
MCTraitDefinition class >> name: classNameString traitComposition:  traitCompositionString category:  categoryString comment:  commentString commentStamp:   commentStamp [
	^ self instanceLike:
		(self new initializeWithName: classNameString 
			traitComposition:  traitCompositionString
			category:  categoryString
			comment:  commentString  
			commentStamp:   commentStamp)

]

{ #category : #comparing }
MCTraitDefinition >> = aDefinition [
	self flag: #traits. "Ugly we harcoded the super superclass method.  We will have to refactor the definition hierarchy"
	
	^ (aDefinition isKindOf: MCDefinition)
		and: [(self isRevisionOf: aDefinition)
		and: [self traitCompositionString = aDefinition traitCompositionString
		and: [category = aDefinition category
		and: [comment = aDefinition comment]]]]
]

{ #category : #visiting }
MCTraitDefinition >> accept: aVisitor [
	^ aVisitor visitTraitDefinition: self

]

{ #category : #installing }
MCTraitDefinition >> createClass [
	| trait |
	trait := Trait
		named: name
		uses: (Smalltalk compiler evaluate: self traitCompositionString)
		category: category.
	trait ifNotNil: [trait classComment: comment stamp: commentStamp].
	^trait
]

{ #category : #testing }
MCTraitDefinition >> hasClassInstanceVariables [
	^ false


]

{ #category : #comparing }
MCTraitDefinition >> hash [
	| hash |
	hash := String stringHash: name initialHash: 0.
	hash := String stringHash: self traitCompositionString initialHash: hash.
	hash := String stringHash: (category ifNil: ['']) initialHash: hash.
	^ hash

]

{ #category : #initializing }
MCTraitDefinition >> initializeWithName: classNameString 
	traitComposition:  traitCompositionString
	category:  categoryString
	comment:  commentString  
	commentStamp:   commentStampString [
					
		name := classNameString asSymbol.
		traitComposition := traitCompositionString.
	     category := categoryString.
		comment := commentString withSqueakLineEndings.
		commentStamp :=  commentStampString ifNil: [self defaultCommentStamp]

]

{ #category : #printing }
MCTraitDefinition >> printClassDefinitionOn: stream [

	^ self printMetaDefinitionOn: stream
]

{ #category : #printing }
MCTraitDefinition >> printDefinitionOn: stream [
	stream nextPutAll: 'Trait named: #', self className;
		 cr;
		 tab;
		 nextPutAll: 'uses: ';
		 nextPutAll: self traitCompositionString;
		 cr;
		 tab;
		 nextPutAll: 'category: ';
		 store: self category asString

]

{ #category : #comparing }
MCTraitDefinition >> requirements [
	"Assuming that traits in a composition can be identified by
	testing for the first character beeing an uppercase character
	(and thus not a special character such as {, # etc.)"

	| tokens traitNames |
	self hasTraitComposition ifFalse: [ ^Array new ].
	tokens := traitComposition parseLiterals.
	traitNames := tokens select: [:each | each first isUppercase].
	^traitNames asArray
]
