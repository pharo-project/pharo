"
I hold tests for DynamicLeftToRightColumnsLayout.
"
Class {
	#name : #SpDynamicLeftToRightColumnsLayoutTest,
	#superclass : #TestCase,
	#instVars : [
		'presentersBuilder',
		'layout'
	],
	#category : #'Spec2-Tests-Dynamic-Builder'
}

{ #category : #running }
SpDynamicLeftToRightColumnsLayoutTest >> setUp [
	super setUp.
	presentersBuilder := SpDynamicPresentersListBuilder new
		modelObjects: (1 to: 14);
		presenter: SpButtonPresenter configuredAs: [ :button :i | "No config. on purpose." ];
		buildDynamicPresenter;
		yourself.
		
	layout := SpDynamicLeftToRightColumnsLayout columns: 7.
]

{ #category : #tests }
SpDynamicLeftToRightColumnsLayoutTest >> testNullPresentersNeededFor [
	self
		assert: (layout nullPresentersNeededFor: presentersBuilder) equals: 0
]
