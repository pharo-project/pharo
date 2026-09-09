"
A MCTraitDefinition represents a trait.
"
Class {
	#name : 'MCTraitDefinition',
	#superclass : 'MCClassDefinition',
	#category : 'Monticello-Model',
	#package : 'Monticello-Model'
}

{ #category : 'comparing' }
MCTraitDefinition >> = aDefinition [

	self flag: #traits. "Ugly we harcoded the super superclass method.  We will have to refactor the definition hierarchy"

	^ self class = aDefinition class and: [
			  (self isRevisionOf: aDefinition) and: [
					  self traitCompositionString = aDefinition traitCompositionString and: [
							  self classTraitCompositionString = aDefinition classTraitCompositionString and: [
									  self category = aDefinition category and: [
											  self slotDefinitionString = aDefinition slotDefinitionString and: [
												  self classInstVarNames = aDefinition classInstVarNames and: [ comment = aDefinition comment ] ] ] ] ] ] ]
]

{ #category : 'accessing' }
MCTraitDefinition >> classDefinitionString [
	"Answer a string describing the class-side definition."

	^String streamContents: [:stream | self printMetaDefinitionOn: stream]
]

{ #category : 'printing' }
MCTraitDefinition >> classSlotDefinitionString [

	^ self needsSlotClassDefinition 
		ifTrue: [ self classInstanceVariables asString ]
		ifFalse: [ 
			String streamContents: [ :stream |
				stream nextPutAll: '{ '.
				self classInstanceVariables 
					do: [ :each | stream print: each name ]
					separatedBy: [ stream nextPutAll: ' . ' ].
				stream nextPutAll: ' }']]
]

{ #category : 'testing' }
MCTraitDefinition >> hasClassInstanceVariables [
	^ false


]

{ #category : 'comparing' }
MCTraitDefinition >> hash [

	^ ((name hash bitXor: traitComposition hash) bitXor: packageName hash) bitXor: tagName hash
]

{ #category : 'testing' }
MCTraitDefinition >> isTraitDefinition [
	^ true
]

{ #category : 'installing' }
MCTraitDefinition >> loadWithLoader: aLoader [

	^ [
	  self class classInstaller make: [ :aBuilder |
		  aBuilder
			  name: name;
			  traitComposition:
				  (OpalEvaluator new evaluate: self traitCompositionString);
			  classTraitComposition:
				  (OpalEvaluator new evaluate: self classTraitCompositionString);
			  slots: self instanceVariables;
			  classSlots: self classInstanceVariables;
			  package: self packageName;
			  tag: self tagName;
			  comment: comment stamp: commentStamp;
			  beTrait.
		  aLoader targetEnvironment ifNotNil: [
			  aBuilder environment: aLoader targetEnvironment ] ] ]
		  on: Warning , DuplicatedVariableError
		  do: [ :ex |
			  aLoader isStrict
				  ifTrue: [ ex pass ]
				  ifFalse: [ ex resume ] ]
]

{ #category : 'printing' }
MCTraitDefinition >> printClassDefinitionOn: stream [

	^ self printMetaDefinitionOn: stream
]

{ #category : 'printing' }
MCTraitDefinition >> printDefinitionOn: stream [
	stream nextPutAll: 'Trait named: #', self className;
		 cr;
		 tab;
		 nextPutAll: 'uses: ';
		 nextPutAll: self traitCompositionString;
		 cr.

	self instanceVariablesString isEmpty ifFalse: [
			stream 
				tab;
				nextPutAll: 'instanceVariableNames: ';
				nextPut: $';
		 		nextPutAll: self instanceVariablesString;
				nextPut: $';
				cr ].
		
	stream
		 tab;
		 nextPutAll: 'package: ';
		 store: self category asString

]

{ #category : 'printing' }
MCTraitDefinition >> printMetaDefinitionOn: stream [

	stream nextPutAll: self className, ' classTrait'.
	self printDefinitionBodyOn: stream
]

{ #category : 'comparing' }
MCTraitDefinition >> requirements [
	"Assuming that traits in a composition can be identified by
	testing for the first character beeing an uppercase character
	(and thus not a special character such as {, # etc.)"

	self hasTraitComposition
		ifFalse: [ ^ #() ].

	^ (((OCParser parseExpression: self traitCompositionString)
		withAllChildren select: [ :e | e isVariable ])
		collect: [ :e | e name ]
		as: Set) asArray
]

{ #category : 'printing' }
MCTraitDefinition >> slotDefinitionString [

	variables ifNil: [ ^ '{ }' ].

	^ self needsSlotClassDefinition 
		ifTrue: [ self instanceVariables asString ]
		ifFalse: [ 
			String streamContents: [ :stream |
				stream nextPutAll: '{ '.
				self instanceVariables 
					do: [ :each | stream print: each name ]
					separatedBy: [ stream nextPutAll: ' . ' ].
				stream nextPutAll: ' }']]
]
