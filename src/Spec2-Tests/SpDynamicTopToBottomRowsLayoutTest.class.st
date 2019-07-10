"
I hold tests for DynamicTopToBottomRowsLayout.
"
Class {
	#name : #SpDynamicTopToBottomRowsLayoutTest,
	#superclass : #TestCase,
	#instVars : [
		'presentersBuilder',
		'layout'
	],
	#category : #'Spec2-Tests-Dynamic-Builder'
}

{ #category : #running }
SpDynamicTopToBottomRowsLayoutTest >> setUp [
	super setUp.
	presentersBuilder := SpDynamicPresentersListBuilder new
		modelObjects: (1 to: 14);
		presenter: SpButtonPresenter configuredAs: [ :button :i | "No config. on purpose." ];
		buildDynamicPresenter;
		yourself.
		
	layout := SpDynamicTopToBottomRowsLayout rows: 7.
]

{ #category : #tests }
SpDynamicTopToBottomRowsLayoutTest >> testNullPresentersNeededFor [
	self
		assert: (layout nullPresentersNeededFor: presentersBuilder) equals: 0
]
