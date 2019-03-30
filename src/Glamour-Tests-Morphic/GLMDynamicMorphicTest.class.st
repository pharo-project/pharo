Class {
	#name : #GLMDynamicMorphicTest,
	#superclass : #GLMMorphicTest,
	#category : #'Glamour-Tests-Morphic'
}

{ #category : #tests }
GLMDynamicMorphicTest >> testSurroundingPanel [
	| browser dynamicContainerMorph |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a | a dynamic presentation: [:some :object | some text ]].
	window := browser openOn: 4.

	dynamicContainerMorph := window submorphs last submorphs first submorphs first.
	self assert: (dynamicContainerMorph isKindOf: PanelMorph).
	self assert: (dynamicContainerMorph submorphs first isKindOf: RubScrolledTextMorph)
]

{ #category : #tests }
GLMDynamicMorphicTest >> testUpdate [
	| browser dynamicContainerMorph presentationObject |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a | 
		a dynamic presentation: [:some :object | 
			some custom: presentationObject ]].
	presentationObject := GLMRubricTextPresentation new.
	window := browser openOn: 42.
	dynamicContainerMorph := window submorphs last submorphs first submorphs first.
	self assert: (dynamicContainerMorph isKindOf: PanelMorph).
	self assert: (dynamicContainerMorph submorphs first isKindOf: RubScrolledTextMorph).
	presentationObject := GLMListPresentation new.
	browser update.
	dynamicContainerMorph := window submorphs last submorphs first submorphs first.
	self assert: (dynamicContainerMorph isKindOf: PanelMorph).
	self assert: (dynamicContainerMorph submorphs first submorphs first isKindOf: MorphTreeMorph)
]

{ #category : #tests }
GLMDynamicMorphicTest >> testWithDisplay [
	<expectedFailure>
	| browser dynamic stacker text textMorph |
	browser := GLMTabulator new column: #stacker; yourself.
	browser transmit to: #stacker; andShow: [:c | 
		c dynamic
			display: [:number | |st|
				st := GLMStacker new.
				st aPane: #all.
				st transmit to: #all; andShow: [:a | a text display: ['All ', number asString]].
				st]].
	window := browser openOn: 10.
	dynamic := browser panes first presentations first.
	stacker := dynamic cachedPresentation first.
	self assert: stacker panes first presentations size equals: 1.
	text := stacker panes first presentations first.
	textMorph := self findWherePresentationIs: text in: window.
	self assert: textMorph text asString equals: 'All 10'
	
]

{ #category : #tests }
GLMDynamicMorphicTest >> testWithPresentation [
	| browser dynamic text textMorph |
	browser := GLMTabulator new column: #stacker; yourself.
	browser transmit to: #stacker; andShow: [:c | 
		c dynamic
			presentation: [:presentation :number |
					presentation text display: ['All ', number asString]]].
	window := browser openOn: 10.
	dynamic := browser panes first presentations first.
	text := dynamic cachedPresentation first.
	textMorph := self findWherePresentationIs: text in: window.
	self assert: textMorph text asString equals: 'All 10'
]
