"
ast := (RBParser parseExpression: 'Trait1 + Trait2 + Trait3 + Trait4').
ast := (RBParser parseExpression: '(Trait2 - {#c})').
ast := (RBParser parseExpression: 'Trait1 + (Trait2 - {#c})').
ast := (RBParser parseExpression: 'Trait1 + (Trait2 - {#c}) + Trait3').
ast := (RBParser parseExpression: 'Trait1 + (Trait2 - #(c ahoj bla: bla:bla:)) + Trait3').

composition := RGTraitComposition unnamed.

visitor := RGTraitCompositionVisitor new.
visitor traitComposition: composition.
ast acceptVisitor: visitor.





"
Class {
	#name : #RGTraitCompositionVisitor,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'traitComposition'
	],
	#category : #'Ring-Core-Parsing'
}

{ #category : #parsing }
RGTraitCompositionVisitor >> parse: anExpressionString for: anRGTraitCompositionDefinition [

	| ast result |
	ast := (RBParser parseExpression: anExpressionString).
	self traitComposition: anRGTraitCompositionDefinition.
	result := ast acceptVisitor: self.
	
	result isCollection 
		ifFalse: [ result := OrderedCollection with: result. ].
		
	^ result.
]

{ #category : #accessing }
RGTraitCompositionVisitor >> traitComposition [
	^ traitComposition
]

{ #category : #accessing }
RGTraitCompositionVisitor >> traitComposition: anObject [
	traitComposition := anObject.
	traitComposition pvtCleanTransformations.
]

{ #category : #visiting }
RGTraitCompositionVisitor >> visitArrayNode: anArrayNode [ 

	^ anArrayNode children collect: [:each | self visitNode: each] 
]

{ #category : #visiting }
RGTraitCompositionVisitor >> visitLiteralArrayNode: aRBLiteralArrayNode [ 

	^ aRBLiteralArrayNode contents collect: [:each | self visitNode: each]
]

{ #category : #visiting }
RGTraitCompositionVisitor >> visitLiteralNode: aLiteralNode [

	^ aLiteralNode value
]

{ #category : #visiting }
RGTraitCompositionVisitor >> visitMessageNode: aMessageNode [ 
	
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
		exclusion := RGTraitExclusion parent: self traitComposition.
		exclusion pvtSubject: rec.
		exclusion pvtCleanExclusions.
		args first do: [:each | exclusion pvtAddExclusion: each ]. 
		^ exclusion].
	
	"an Array(Trait2 @ {#c3->#c2})"
	(aMessageNode selector = #@) ifTrue: [ 
		| exclusion | 
		exclusion := RGTraitAlias parent: self traitComposition.
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
RGTraitCompositionVisitor >> visitSequenceNode: aSequenceNode [ 

	^ aSequenceNode statements collect: [:each | self visitNode: each].
]

{ #category : #visiting }
RGTraitCompositionVisitor >> visitVariableNode: aVariableNode [
	
	^ self traitComposition environment ensureTraitNamed: aVariableNode name

]
