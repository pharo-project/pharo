"
ast := (RBParser parseExpression: 'Trait1 + Trait2 + Trait3 + Trait4').
ast := (RBParser parseExpression: '(Trait2 - {#c})').
ast := (RBParser parseExpression: 'Trait1 + (Trait2 - {#c})').
ast := (RBParser parseExpression: 'Trait1 + (Trait2 - {#c}) + Trait3').
ast := (RBParser parseExpression: 'Trait1 + (Trait2 - #(c ahoj bla: bla:bla:)) + Trait3').

composition := RG2TraitCompositionDefinition unnamed.

visitor := RG2TraitCompositionVisitor new.
visitor traitComposition: composition.
ast acceptVisitor: visitor.





"
Class {
	#name : #RG2TraitCompositionVisitor,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'traitComposition'
	],
	#category : #'Ring2-Core-Parsing'
}

{ #category : #parsing }
RG2TraitCompositionVisitor >> parse: anExpressionString for: aRGTraitCompositionDefinition [

	| ast result |
	ast := (RBParser parseExpression: anExpressionString).
	self traitComposition: aRGTraitCompositionDefinition.
	result := ast acceptVisitor: self.
	
	result isCollection 
		ifFalse: [ result := OrderedCollection with: result. ].
		
	^ result.
]

{ #category : #accessing }
RG2TraitCompositionVisitor >> traitComposition [
	^ traitComposition
]

{ #category : #accessing }
RG2TraitCompositionVisitor >> traitComposition: anObject [
	traitComposition := anObject.
	traitComposition pvtCleanTransformations.
]

{ #category : #visiting }
RG2TraitCompositionVisitor >> visitArrayNode: anArrayNode [ 

	^ anArrayNode children collect: [:each | self visitNode: each] 
]

{ #category : #visiting }
RG2TraitCompositionVisitor >> visitLiteralArrayNode: aRBLiteralArrayNode [ 

	^ aRBLiteralArrayNode contents collect: [:each | self visitNode: each]
]

{ #category : #visiting }
RG2TraitCompositionVisitor >> visitLiteralNode: aLiteralNode [

	^ aLiteralNode value
]

{ #category : #visiting }
RG2TraitCompositionVisitor >> visitMessageNode: aMessageNode [ 
	
	| rec args | 
	rec := (self visitNode: aMessageNode receiver).
	args := (aMessageNode arguments collect: [ :each | self visitNode: each ]).

	(aMessageNode selector = #+) ifTrue: [
		rec isCollection 
			ifTrue: [ rec add: args first ]
			ifFalse: [ rec := { rec. args first } asOrderedCollection ].
		^ rec].
	
	(aMessageNode selector = #-) ifTrue: [ 
		| exclusion | 
		exclusion := RG2TraitExclusionDefinition parent: self traitComposition.
		exclusion pvtSubject: rec.
		exclusion pvtCleanExclusions.
		args first do: [:each | exclusion pvtAddExclusion: each ]. 
		^ exclusion].
	
	"an Array(Trait2 @ {#c3->#c2})"
	(aMessageNode selector = #@) ifTrue: [ 
		| exclusion | 
		exclusion := RG2TraitAliasDefinition parent: self traitComposition.
		exclusion pvtSubject: rec.
		exclusion pvtCleanAliases.
		args first do: [:each | exclusion pvtAddAlias: each ]. 
		^ exclusion].

	(aMessageNode selector = #->) ifTrue: [
		"for associations in aliases"
		^ rec -> args first
		].
	
	(aMessageNode selector = #classTrait) ifTrue: [
		"for associations in aliases"
		rec classTrait makeResolved.
		rec classTrait name: rec name, ' classTrait'.
		^ rec classTrait.
		].

	self error: 'Unknown trait composition message'.
]

{ #category : #visiting }
RG2TraitCompositionVisitor >> visitSequenceNode: aSequenceNode [ 

	^ aSequenceNode statements collect: [:each | self visitNode: each].
]

{ #category : #visiting }
RG2TraitCompositionVisitor >> visitVariableNode: aVariableNode [
	
	^ self traitComposition environment ensureTraitNamed: aVariableNode name

]
