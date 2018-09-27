"
I hold tests for DynamicTopToBottomRowsLayout.
"
Class {
	#name : #DynamicTopToBottomRowsLayoutTest,
	#superclass : #TestCase,
	#instVars : [
		'presentersBuilder',
		'layout'
	],
	#category : #'Spec-Tests-Dynamic-Builder'
}

{ #category : #running }
DynamicTopToBottomRowsLayoutTest >> setUp [
	super setUp.
	presentersBuilder := DynamicPresentersListBuilder new
		modelObjects: (1 to: 14);
		presenter: ButtonPresenter configuredAs: [ :button :i | "No config. on purpose." ];
		buildDynamicPresenter;
		yourself.
		
	layout := DynamicTopToBottomRowsLayout rows: 7.
]

{ #category : #tests }
DynamicTopToBottomRowsLayoutTest >> testNullPresentersNeededFor [
	self
		assert: (layout nullPresentersNeededFor: presentersBuilder) equals: 0
]
