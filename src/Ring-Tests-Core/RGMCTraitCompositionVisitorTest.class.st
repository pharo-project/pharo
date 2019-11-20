Class {
	#name : #RGMCTraitCompositionVisitorTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGMCTraitCompositionVisitorTest >> testAlias [

	| composition result |

	composition := RGTraitComposition unnamed.
	result := RGTraitCompositionVisitor new parse: 'Trait2 @ {#a -> #c}' for: composition.
	
	self assert: result isCollection.
	self assert: (result allSatisfy: #isTraitAlias).
	self assert: (result allSatisfy: #isRGObject).
	self assert: result size equals: 1.
	self assert: result first subject isTrait.	
	self assert: result first subject name equals: #Trait2.
	self assert: result first aliases isCollection.
	self assert: result first aliases first equals: (#a -> #c).
	
]

{ #category : #tests }
RGMCTraitCompositionVisitorTest >> testAliasWithMultipleAliases [

	| composition result |

	composition := RGTraitComposition unnamed.
	result := RGTraitCompositionVisitor new parse: 'Trait2 @ {#a -> #b. #c: -> #d:}' for: composition.
	
	self assert: result isCollection.
	self assert: (result allSatisfy: #isTraitAlias).
	self assert: (result allSatisfy: #isRGObject).
	self assert: result size equals: 1.
	self assert: result first subject isTrait.	
	self assert: result first subject name equals: #Trait2.
	self assert: result first aliases isCollection.
	self assert: result first aliases size equals: 2.
	self assert: result first aliases first equals: (#a -> #b).
	self assert: result first aliases second equals: (#c: -> #d:).
	
]

{ #category : #tests }
RGMCTraitCompositionVisitorTest >> testClassTrait [

	| composition result |

	composition := RGTraitComposition unnamed.
	result := RGTraitCompositionVisitor new parse: 'Trait1 classTrait + Trait2 classTrait' for: composition.
	
	self assert: result isCollection.
	self assert: (result allSatisfy: #isTrait).
	self assert: (result allSatisfy: #isRGObject).
	self assert: (result collect: #name) equals: #('Trait1 classTrait' 'Trait2 classTrait') asOrderedCollection.
]

{ #category : #tests }
RGMCTraitCompositionVisitorTest >> testComplexDefinition [
	| composition result |
	composition := RGTraitComposition unnamed.
	result := RGTraitCompositionVisitor new parse: 'Trait1 + (Trait2 @ {#a -> #b. #c: -> #d:}) + (Trait3 - {#e. #f:})' for: composition.

	self assert: result isCollection.
	self assert: (result allSatisfy: #isRGObject).
	self assert: result size equals: 3.

	self assert: result first isTrait.
	self assert: result first name equals: #Trait1.

	self assert: result second isTraitAlias.
	self assert: result second subject isTrait.
	self assert: result second subject name equals: #Trait2.
	self assert: result second aliases isCollection.
	self assert: result second aliases size equals: 2.
	self assert: result second aliases first equals: #a -> #b.
	self assert: result second aliases second equals: #c: -> #d:.

	self assert: result third isTraitExclusion.
	self assert: result third isRGObject.
	self assert: result third subject isTrait.
	self assert: result third subject name equals: #Trait3.
	self assert: result third exclusions isCollection.
	self assert: result third exclusions asArray equals: #(#e #f:)
]

{ #category : #tests }
RGMCTraitCompositionVisitorTest >> testEmpty [

	| composition result |

	composition := RGTraitComposition unnamed.
	result := RGTraitCompositionVisitor new parse: '' for: composition.
	
	self assert: result isCollection.
	self assert: result isEmpty.
	
	composition := RGTraitComposition unnamed.
	result := RGTraitCompositionVisitor new parse: '{}' for: composition.
	
	self assert: result isCollection.
	self assert: result isEmpty.
]

{ #category : #tests }
RGMCTraitCompositionVisitorTest >> testExclusion [
	| composition result |
	composition := RGTraitComposition unnamed.
	result := RGTraitCompositionVisitor new parse: 'Trait2 - {#c}' for: composition.

	self assert: result isCollection.
	self assert: (result allSatisfy: #isTraitExclusion).
	self assert: (result allSatisfy: #isRGObject).
	self assert: result size equals: 1.
	self assert: result first subject isTrait.
	self assert: result first subject name equals: #Trait2.
	self assert: result first exclusions isCollection.
	self assert: result first exclusions asArray equals: #(#c)
]

{ #category : #tests }
RGMCTraitCompositionVisitorTest >> testExclusionWithLiteralArray [
	| composition result |
	composition := RGTraitComposition unnamed.
	result := RGTraitCompositionVisitor new parse: 'Trait2 - #(c)' for: composition.

	self assert: result isCollection.
	self assert: (result allSatisfy: #isTraitExclusion).
	self assert: (result allSatisfy: #isRGObject).
	self assert: result size equals: 1.
	self assert: result first subject isTrait.
	self assert: result first subject name equals: #Trait2.
	self assert: result first exclusions isCollection.
	self assert: result first exclusions asArray equals: #(#c)
]

{ #category : #tests }
RGMCTraitCompositionVisitorTest >> testExclusionWithMultipleExclusions [
	| composition result |
	composition := RGTraitComposition unnamed.
	result := RGTraitCompositionVisitor new parse: 'Trait2 - {#a. #b:. #c:d:}' for: composition.

	self assert: result isCollection.
	self assert: (result allSatisfy: #isTraitExclusion).
	self assert: (result allSatisfy: #isRGObject).
	self assert: result size equals: 1.
	self assert: result first subject isTrait.
	self assert: result first subject name equals: #Trait2.
	self assert: result first exclusions isCollection.
	self assert: result first exclusions asArray equals: #(#a #b: #c:d:)
]

{ #category : #tests }
RGMCTraitCompositionVisitorTest >> testMultipleTraits [

	| composition result |

	composition := RGTraitComposition unnamed.
	result := RGTraitCompositionVisitor new parse: 'Trait1 + Trait2 + Trait3 + Trait4' for: composition.
	
	self assert: result isCollection.
	self assert: result size equals: 4.

	self assert: (result allSatisfy: #isTrait).
	self assert: (result allSatisfy: #isRGObject).
	self assert: (result collect: #name) equals: #(Trait1 Trait2 Trait3 Trait4) asOrderedCollection.
]

{ #category : #tests }
RGMCTraitCompositionVisitorTest >> testSingleTrait [

	| composition result |

	composition := RGTraitComposition unnamed.
	result := RGTraitCompositionVisitor new parse: 'Trait1' for: composition.
	
	self assert: result isCollection.
	self assert: (result allSatisfy: #isTrait).
	self assert: (result allSatisfy: #isRGObject).
	self assert: (result collect: #name) equals: #(Trait1) asOrderedCollection.
]

{ #category : #tests }
RGMCTraitCompositionVisitorTest >> testTwoTraits [

	| composition result |

	composition := RGTraitComposition unnamed.
	result := RGTraitCompositionVisitor new parse: 'Trait1 + Trait2' for: composition.
	
	self assert: result isCollection.
	self assert: (result allSatisfy: #isTrait).
	self assert: (result allSatisfy: #isRGObject).
	self assert: (result collect: #name) equals: #(Trait1 Trait2) asOrderedCollection.
]
