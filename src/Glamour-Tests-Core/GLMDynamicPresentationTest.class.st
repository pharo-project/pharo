Class {
	#name : #GLMDynamicPresentationTest,
	#superclass : #TestCase,
	#category : #'Glamour-Tests-Core'
}

{ #category : #tests }
GLMDynamicPresentationTest >> testCachedPresentation [
	| dynamic currentPresentation |
	dynamic := GLMDynamicPresentation new.
	dynamic entity: 42.
	dynamic transformation: [ :entity | GLMRubricTextPresentation new ].
	currentPresentation := dynamic currentPresentation.

	self assert: dynamic cachedPresentation identicalTo: currentPresentation
]

{ #category : #tests }
GLMDynamicPresentationTest >> testDefaultPresentation [
	| dynamic |
	dynamic := GLMDynamicPresentation new.
	dynamic entity: 42.
	dynamic transformation: [ :entity | GLMRubricTextPresentation new ].
	self assert: dynamic currentPresentation presentations size equals: 1.
	self assert: (dynamic currentPresentation presentations first isKindOf: GLMRubricTextPresentation).
	self assert: dynamic currentPresentation pane identicalTo: dynamic pane
]

{ #category : #tests }
GLMDynamicPresentationTest >> testDirectEventsPropagationTransformation [
	| dynamic textPresentation counter portOne portTwo |
	counter := 0.
	dynamic := GLMDynamicPresentation new.
	dynamic entity: '42'.
	dynamic display: [ :entity | textPresentation := GLMRubricTextPresentation new ].
	
	"Build the presentation"
	dynamic currentPresentation.
	
	textPresentation announcer when: GLMContextChanged do: [ :ann |
		counter := counter + 1 ].
	portOne := (GLMSimplePort new) name: #portOne; value: 1.
	portTwo := (GLMSimplePort new) name: #portTwo; value: 2.
	dynamic outerPortEvent: (GLMPortEvent on: portOne previouslyValued: portTwo).
	
	self assert: counter equals: 1.
]

{ #category : #tests }
GLMDynamicPresentationTest >> testIndirectEventsPropagationTransformation [
	| dynamic textPresentation counter |
	counter := 0.
	dynamic := GLMDynamicPresentation new.
	dynamic entity: '42'.
	dynamic display: [ :entity | textPresentation := GLMRubricTextPresentation new ].
	"Build the presentation"
	dynamic currentPresentation.
	
	textPresentation announcer when: GLMContextChanged do: [ :ann |
		ann property = #selectionInterval
			ifTrue: [ counter := counter + 1 ] ].
	textPresentation selectionInterval: (1 to: 2).
	
	self assert: counter equals: 1.
]

{ #category : #tests }
GLMDynamicPresentationTest >> testPresentationInitializationBlock [
	| dynamic count |
	count := 0.
	dynamic := GLMDynamicPresentation new.
	dynamic
		transformation: [ :entity | 
			GLMListPresentation new
				initialize: [ :pres | 
					count := count + 1.
					pres selection: 5 ] ].
	dynamic entity: (1 to: 42).
	self assert: count equals: 0.
	self assert: dynamic currentPresentation presentations first selection equals: 5.
	self assert: count equals: 1
]

{ #category : #tests }
GLMDynamicPresentationTest >> testPresentationTransformation [
	| dynamic |
	dynamic := GLMDynamicPresentation new.
	dynamic entity: 42.
	dynamic presentationTransformation: [ :some :entity | (1 to: entity) do: [ :each | some text title: each asString ] ].
	self assert: dynamic currentPresentation presentations size equals: 42.
	self assert: dynamic currentPresentation pane identicalTo: dynamic pane
]
