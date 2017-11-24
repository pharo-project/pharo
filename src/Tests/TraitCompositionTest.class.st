Class {
	#name : #TraitCompositionTest,
	#superclass : #TraitsTestCase,
	#category : #'Tests-Traits'
}

{ #category : #'testing-basic' }
TraitCompositionTest >> testAliasCompositions [
	"unary"

	self t2 setTraitCompositionFrom: self t1 @ {(#aliasM11 -> #m11)}.
	self should: [ self t2 setTraitCompositionFrom: self t1 @ {(#alias: -> #m11)} ] raise: TraitCompositionException.
	self should: [ self t2 setTraitCompositionFrom: self t1 @ {(#alias:x:y: -> #m11)} ] raise: TraitCompositionException.	"binary"
	self t1 compile: '= anObject'.
	self t2 setTraitCompositionFrom: self t1 @ {(#equals: -> #=)}.
	self t2 setTraitCompositionFrom: self t1 @ {(#% -> #=)}.
	self should: [ self t2 setTraitCompositionFrom: self t1 @ {(#equals -> #=)} ] raise: TraitCompositionException.
	self should: [ self t2 setTraitCompositionFrom: self t1 @ {(#equals:x: -> #=)} ] raise: TraitCompositionException.	"keyword"
	self t1 compile: 'x: a y: b z: c'.
	self should: [ self t2 setTraitCompositionFrom: self t1 @ {(#== -> #x:y:z:)} ] raise: TraitCompositionException.
	self should: [ self t2 setTraitCompositionFrom: self t1 @ {(#x -> #x:y:z:)} ] raise: TraitCompositionException.
	self should: [ self t2 setTraitCompositionFrom: self t1 @ {(#x: -> #x:y:z:)} ] raise: TraitCompositionException.
	self should: [ self t2 setTraitCompositionFrom: self t1 @ {(#x:y: -> #x:y:z:)} ] raise: TraitCompositionException.
	self t2 setTraitCompositionFrom: self t1 @ {(#myX:y:z: -> #x:y:z:)}.	"alias same as selector"
	self should: [ self t2 setTraitCompositionFrom: self t1 @ {(#m11 -> #m11)} ] raise: TraitCompositionException.	"same alias name used twice"
	self
		should: [ 
			self t2
				setTraitCompositionFrom:
					self t1
						@
							{(#alias -> #m11).
							(#alias -> #m12)} ]
		raise: TraitCompositionException.	"aliasing an alias"
	self
		should: [ 
			self t2
				setTraitCompositionFrom:
					self t1
						@
							{(#alias -> #m11).
							(#alias2 -> #alias)} ]
		raise: TraitCompositionException
]

{ #category : #'testing-enquiries' }
TraitCompositionTest >> testClassMethodsTakePrecedenceOverTraitsMethods [
	| keys |
	keys := Set new.
	self t4 methodDict bindingsDo: [:each | keys add: each key].
	self assert: keys size = 6.
	self 
		assert: (keys includesAll: #(
						#m12
						#m13
						#m13
						#m21
						#m22
						#m11
						#m42
					)).
	self assert: (self t4 methodDict at: #m11) sourceCode = 'm11 ^41'
]

{ #category : #'testing-basic' }
TraitCompositionTest >> testCompositionFromArray [
	| composition |
	composition := { (self t1) } asTraitComposition.
	self assert: (composition isKindOf: TraitComposition).
	self assert: (composition traits includes: self t1).
	self assert: composition traits size = 1.
	composition := { (self t1). self t2 } asTraitComposition.
	self assert: (composition isKindOf: TraitComposition).
	self assert: (composition traits includes: self t1).
	self assert: (composition traits includes: self t2).
	self assert: composition traits size = 2
]

{ #category : #'testing-basic' }
TraitCompositionTest >> testEmptyTrait [
	| composition |
	composition := {} asTraitComposition.
	
	self assert: (composition isKindOf: TraitComposition).
	self assert: composition transformations isEmpty.
	self assert: composition traits isEmpty
]

{ #category : #'testing-basic' }
TraitCompositionTest >> testInvalidComposition [
	self t1 @ {(#a -> #b)} @ {(#x -> #y)}.
	(self t1 + self t2) @ {(#a -> #b)} @ {(#x -> #y)}.
	self t1 - {#a} - {#b}.
	self t1 + self t2 - {#a} - {#b}.
	self should: [ (self t1 - {#x}) @ {(#a -> #b)} ] raise: TraitCompositionException.
	self should: [ (self t1 + self t2 - {#x}) @ {(#a -> #b)} ] raise: TraitCompositionException.
	self should: [ self t1 + self t1 ] raise: TraitCompositionException.
	self should: [ (self t1 + self t2) @ {(#a -> #b)} + self t1 ] raise: TraitCompositionException.
	self
		should: [ 
			self t1
				@
					{(#a -> #m11).
					(#a -> #m12)} ]
		raise: TraitCompositionException.
	self
		should: [ 
			self t1
				@
					{(#a -> #m11).
					(#b -> #a)} ]
		raise: TraitCompositionException
]

{ #category : #'testing-basic' }
TraitCompositionTest >> testOriginSelectorOf [
	
	"regular method "
	self assert: (MOPTestClassC traitComposition originSelectorOf: #c) equals: #c.	
	"non aliased trait method "
	self assert: (MOPTestClassD traitComposition originSelectorOf: #c2) equals: #c2.	
	"aliased method "
	self assert: (MOPTestClassD traitComposition originSelectorOf: #c3) equals: #c2.
]

{ #category : #'testing-basic' }
TraitCompositionTest >> testPrinting [
	| composition1 composition2 |
	composition1 := ((self t2 + self t1) @ { (#z -> #c) } - {#a. #b. #c } 
				+ self t3 - { #d. #e } 
				+ self t4).
	composition2 := self t4 @ { (#x -> #a). (#y -> #b) } + self t1 - { #a } 
				+ self t3 - { #d. #e } 
				+ self t2 - { #b. #c }.
	self assertPrints: composition1 printString
		like: 'T2 @ {#z->#c} - {#d. #e. #a. #b. #c} + (T1 @ {#z->#c} - {#d. #e. #a. #b. #c}) + (T3 - {#d. #e}) + T4'.
	self assertPrints: composition2 printString
		like: 'T4 @ {#x->#a. #y->#b} - {#b. #c. #d. #e. #a} + (T1 - {#b. #c. #d. #e. #a}) + (T3 - {#b. #c. #d. #e}) + (T2 - {#b. #c})'
		
		
]

{ #category : #'testing-basic' }
TraitCompositionTest >> testSum [
	| composition |
	composition := self t1 + self t2 + self t3.
	self assert: (composition isKindOf: TraitComposition).
	self assert: (composition traits includes: self t1).
	self assert: (composition traits includes: self t2).
	self assert: (composition traits includes: self t3).
	self assert: composition traits size = 3
]

{ #category : #'testing-basic' }
TraitCompositionTest >> testSumWithParenthesis [
	| composition |
	composition := self t1 + (self t2 + self t3).
	self assert: (composition isKindOf: TraitComposition).
	self assert: (composition traits includes: self t1).
	self assert: (composition traits includes: self t2).
	self assert: (composition traits includes: self t3).
	self assert: composition traits size = 3.
	self assert: composition size = 3
]

{ #category : #'testing-comparing' }
TraitCompositionTest >> testSyntacticEqualsWithAlias [

	self assert: ((self t1 @ {#a -> #b}) syntacticallyEquals: (self t1 @ {#a -> #b})).
	self deny: ((self t1 @ {#a -> #b}) syntacticallyEquals: self t1).
	self deny: ((self t1 @ {#a -> #b}) syntacticallyEquals: (self t1 @ {#a -> #B})).
	self deny: ((self t1 @ {#a -> #b}) syntacticallyEquals: (self t1 @ {#A -> #b})).

	self assert: ((self t1 @ {#a -> #b} @ {#x -> #y}) syntacticallyEquals: (self t1 @ {#a -> #b} @ {#x -> #y})).
	self deny: ((self t1 @ {#a -> #b} @ {#x -> #y}) syntacticallyEquals: (self t1 @ {#x -> #y} @ {#a -> #b})).
]

{ #category : #'testing-comparing' }
TraitCompositionTest >> testSyntacticEqualsWithComplexCompositions [

	self assert: ((self t1 + (self t2 - {#a. #b}) + (self t3 @ {#x -> #y})) 
		syntacticallyEquals: (self t1 + (self t2 - {#a. #b}) + (self t3 @ {#x -> #y}))).
	self deny: ((self t1 + (self t2 - {#a. #b}) + (self t3 @ {#x -> #y})) 
		syntacticallyEquals: (self t1 + (self t2 - {#A. #B}) + (self t3 @ {#X -> #Y}))).
	
	self assert: ((self t1 @ {#x -> #y} @ {#z -> #w} - {#a. #b} - {#c} + self t2) 
		syntacticallyEquals: (self t1 @ {#x -> #y} @ {#z -> #w} - {#a. #b} - {#c} + self t2)).
	self deny: ((self t1 @ {#x -> #y} @ {#z -> #w} - {#a. #b} - {#c} + self t2) 
		syntacticallyEquals: (self t1 @ {#x -> #y} @ {#z -> #w} - {#a. #b} - {#C} + self t2)).

]

{ #category : #'testing-comparing' }
TraitCompositionTest >> testSyntacticEqualsWithComposition [

	self assert: ({} asTraitComposition syntacticallyEquals: {} asTraitComposition).
	self deny: ({} asTraitComposition syntacticallyEquals: self t1 asTraitComposition).

	self assert: (self t1 asTraitComposition syntacticallyEquals: self t1 asTraitComposition).
	self deny: (self t1 asTraitComposition syntacticallyEquals: self t2 asTraitComposition).
	
	self assert: ((self t1 + self t2) syntacticallyEquals: (self t1 + self t2)).
	self deny: ((self t1 + self t2) syntacticallyEquals: (self t1 + self t3)).
	self deny: ((self t1 + self t2) syntacticallyEquals: (self t2 + self t1)).
	
	self assert: ((self t1 + self t2 + self t3) syntacticallyEquals: (self t1 + self t2 + self t3)).
	self deny: ((self t1 + self t2 + self t3) syntacticallyEquals: (self t3 + self t2 + self t1)).

]

{ #category : #'testing-comparing' }
TraitCompositionTest >> testSyntacticEqualsWithExclusion [

	self assert: ((self t1 - {#a}) syntacticallyEquals: (self t1 - {#a})).
	self deny: ((self t1 - {#a}) syntacticallyEquals: (self t1 - {#b})).
	self deny: ((self t1 - {#a. #b}) syntacticallyEquals: (self t1 - {#b. #a})).
	
	self assert: ((self t1 - {#a} - {#b}) syntacticallyEquals: (self t1 - {#a} - {#b})).
	self deny: ((self t1 - {#a} - {#b}) syntacticallyEquals: (self t1 - {#b} - {#a})).

]
