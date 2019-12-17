Class {
	#name : #TraitCompositionTest,
	#superclass : #TraitsTestCase,
	#category : #'Tests-Traits'
}

{ #category : #'testing-basic' }
TraitCompositionTest >> testAliasCompositions [
	"unary"

	self t2 setTraitComposition: self t1 @ {(#aliasM11 -> #m11)}.
	self should: [ self t2 setTraitComposition: self t1 @ {(#alias: -> #m11)} ] raise: Error.
	self should: [ self t2 setTraitComposition: self t1 @ {(#alias:x:y: -> #m11)} ] raise: Error.	"binary"
	self t1 compile: '= anObject'.
	self t2 setTraitComposition: self t1 @ {(#equals: -> #=)}.
	self t2 setTraitComposition: self t1 @ {(#% -> #=)}.
	self should: [ self t2 setTraitComposition: self t1 @ {(#equals -> #=)} ] raise: Error.
	self should: [ self t2 setTraitComposition: self t1 @ {(#equals:x: -> #=)} ] raise: Error.	"keyword"
	self t1 compile: 'x: a y: b z: c'.
	self should: [ self t2 setTraitComposition: self t1 @ {(#== -> #x:y:z:)} ] raise: Error.
	self should: [ self t2 setTraitComposition: self t1 @ {(#x -> #x:y:z:)} ] raise: Error.
	self should: [ self t2 setTraitComposition: self t1 @ {(#x: -> #x:y:z:)} ] raise: Error.
	self should: [ self t2 setTraitComposition: self t1 @ {(#x:y: -> #x:y:z:)} ] raise: Error.
	self t2 setTraitComposition: self t1 @ {(#myX:y:z: -> #x:y:z:)}.	"alias same as selector"
	self should: [ self t2 setTraitComposition: self t1 @ {(#m11 -> #m11)} ] raise: Error.	"same alias name used twice"
	self
		should: [ 
			self t2
				setTraitCompositionFrom:
					self t1
						@
							{(#alias -> #m11).
							(#alias -> #m12)} ]
		raise: Error.	"aliasing an alias"
	self
		should: [ 
			self t2
				setTraitCompositionFrom:
					self t1
						@
							{(#alias -> #m11).
							(#alias2 -> #alias)} ]
		raise: Error
]

{ #category : #'testing-enquiries' }
TraitCompositionTest >> testClassMethodsTakePrecedenceOverTraitsMethods [
	| keys |
	keys := Set new.
	self t4 methodDict bindingsDo: [ :each | keys add: each key ].
	self assert: keys size equals: 6.
	self assert: (keys includesAll: #(#m12 #m13 #m13 #m21 #m22 #m11 #m42)).
	self assert: (self t4 methodDict at: #m11) sourceCode equals: 'm11 ^41'
]

{ #category : #'testing-basic' }
TraitCompositionTest >> testCompositionFromArray [
	| composition |
	composition := {self t1} asTraitComposition.
	self assert: (composition traits includes: self t1).
	self assert: composition traits size equals: 1.

	composition := {self t1 . self t2} asTraitComposition.
	self assert: (composition traits includes: self t1).
	self assert: (composition traits includes: self t2).
	self assert: composition traits size equals: 2
]

{ #category : #'testing-basic' }
TraitCompositionTest >> testEmptyTrait [
	| composition |
	composition := {} asTraitComposition.

	self assertEmpty: composition.
	self assertEmpty: composition traits
]

{ #category : #'testing-basic' }
TraitCompositionTest >> testInvalidComposition [
	self t1 @ {(#a -> #b)} @ {(#x -> #y)}.
	self shouldnt: [ (self t1 + self t2) @ {(#a -> #b)} @ {(#x -> #y)}] raise: Error.
	self t1 - {#a} - {#b}.
	self t1 + self t2 - {#a} - {#b}.
	self shouldnt: [ (self t1 - {#x}) @ {(#a -> #b)} ] raise: Error.
	self shouldnt: [ (self t1 + self t2 - {#x}) @ {(#a -> #b)} ] raise: Error.
	self should: [ self t1 + self t1 ] raise: Error.
	self shouldnt: [ (self t1 + self t2) @ {(#a -> #b)} + self t1 ] raise: Error.
	self
		should: [ 
			self t1
				@
					{(#a -> #m11).
					(#a -> #m12)} ]
		raise: Error.
	self
		should: [ 
			self t1
				@
					{(#a -> #m11).
					(#b -> #a)} ]
		raise: Error
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
 		like:  '(((T2 + T1) @ {#z->#c}) - {#a. #b. #c} + T3) - {#d. #e} + T4'.
	self assertPrints: composition2 printString
		like:  '(((T4 @ {#x->#a. #y->#b} + T1) - {#a} + T3) - {#d. #e} + T2) - {#b. #c}'
		
		
]

{ #category : #'testing-basic' }
TraitCompositionTest >> testSum [
	| composition |
	composition := self t1 + self t2 + self t3.
	self assert: (composition isKindOf: TaSequence).
	self assert: (composition traits includes: self t1).
	self assert: (composition traits includes: self t2).
	self assert: (composition traits includes: self t3).
	self assert: composition traits size equals: 3
]

{ #category : #'testing-basic' }
TraitCompositionTest >> testSumWithParenthesis [
	| composition |
	composition := self t1 + (self t2 + self t3).
	self assert: (composition traits includes: self t1).
	self assert: (composition traits includes: self t2).
	self assert: (composition traits includes: self t3).
	self assert: composition traits size equals: 3
]

{ #category : #'testing-comparing' }
TraitCompositionTest >> testSyntacticEqualsWithAlias [

	"To create a trait composition, it should be valid."
	self t1 compile: 'b ^$b'.
	self t1 compile: 'B ^$B'.
	self t1 compile: 'y ^$y'.

	self assert: ((self t1 @ {#a -> #b}) semanticallyEquals: (self t1 @ {#a -> #b})).
	self deny: ((self t1 @ {#a -> #b}) semanticallyEquals: self t1).
	self deny: ((self t1 @ {#a -> #b}) semanticallyEquals: (self t1 @ {#a -> #B})).
	self deny: ((self t1 @ {#a -> #b}) semanticallyEquals: (self t1 @ {#A -> #b})).

	self assert: ((self t1 @ {#a -> #b} @ {#x -> #y}) semanticallyEquals: (self t1 @ {#a -> #b} @ {#x -> #y})).
	self deny: ((self t1 @ {#a -> #b} @ {#x -> #y}) semanticallyEquals: (self t1 @ {#x -> #y} @ {#a -> #b})).
]

{ #category : #'testing-comparing' }
TraitCompositionTest >> testSyntacticEqualsWithComplexCompositions [

	self t3 compile: 'y ^$y'.
	self t3 compile: 'Y ^$Y'.

	self assert: ((self t1 + (self t2 - {#a. #b}) + (self t3 @ {#x -> #y})) 
		semanticallyEquals: (self t1 + (self t2 - {#a. #b}) + (self t3 @ {#x -> #y}))).
	self deny: ((self t1 + (self t2 - {#a. #b}) + (self t3 @ {#x -> #y})) 
		semanticallyEquals: (self t1 + (self t2 - {#A. #B}) + (self t3 @ {#X -> #Y}))).

	self t1 compile: 'y ^$y'.
	self t1 compile: 'w ^$w'.
	self t1 compile: 'c ^$c'.
	
	self assert: ((self t1 @ {#x -> #y} @ {#z -> #w} - {#a. #b} - {#c} + self t2) 
		semanticallyEquals: (self t1 @ {#x -> #y} @ {#z -> #w} - {#a. #b} - {#c} + self t2)).
	self deny: ((self t1 @ {#x -> #y} @ {#z -> #w} - {#a. #b} - {#c} + self t2) 
		semanticallyEquals: (self t1 @ {#x -> #y} @ {#z -> #w} - {#a. #b} - {#C} + self t2)).

]

{ #category : #'testing-comparing' }
TraitCompositionTest >> testSyntacticEqualsWithComposition [

	self assert: ({} asTraitComposition semanticallyEquals: {} asTraitComposition).
	self deny: ({} asTraitComposition semanticallyEquals: self t1 asTraitComposition).

	self assert: (self t1 asTraitComposition semanticallyEquals: self t1 asTraitComposition).
	self deny: (self t1 asTraitComposition semanticallyEquals: self t2 asTraitComposition).
	
	self assert: ((self t1 + self t2) semanticallyEquals: (self t1 + self t2)).
	self deny: ((self t1 + self t2) semanticallyEquals: (self t1 + self t3)).
	self deny: ((self t1 + self t2) semanticallyEquals: (self t2 + self t1)).
	
	self assert: ((self t1 + self t2 + self t3) semanticallyEquals: (self t1 + self t2 + self t3)).
	self deny: ((self t1 + self t2 + self t3) semanticallyEquals: (self t3 + self t2 + self t1)).

]

{ #category : #'testing-comparing' }
TraitCompositionTest >> testSyntacticEqualsWithExclusion [

	self t1 compile: 'a ^$a'.
	self t1 compile: 'b ^$b'.

	self assert: ((self t1 - {#a}) semanticallyEquals: (self t1 - {#a})).
	self deny: ((self t1 - {#a}) semanticallyEquals: (self t1 - {#b})).
	self assert: ((self t1 - {#a. #b}) semanticallyEquals: (self t1 - {#b. #a})).
	
	self assert: ((self t1 - {#a} - {#b}) semanticallyEquals: (self t1 - {#a} - {#b})).
	self assert: ((self t1 - {#a} - {#b}) semanticallyEquals: (self t1 - {#b} - {#a})).

]
