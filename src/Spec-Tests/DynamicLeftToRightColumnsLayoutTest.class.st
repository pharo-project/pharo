"
I hold tests for DynamicLeftToRightColumnsLayout.
"
Class {
	#name : #DynamicLeftToRightColumnsLayoutTest,
	#superclass : #TestCase,
	#instVars : [
		'presentersBuilder',
		'layout'
	],
	#category : #'Spec-Tests-Dynamic-Builder'
}

{ #category : #running }
DynamicLeftToRightColumnsLayoutTest >> setUp [
	super setUp.
	presentersBuilder := DynamicPresentersListBuilder new
		modelObjects: (1 to: 14);
		presenter: ButtonPresenter configuredAs: [ :button :i | "No config. on purpose." ];
		buildDynamicPresenter;
		yourself.
		
	layout := DynamicLeftToRightColumnsLayout columns: 7.
]

{ #category : #tests }
DynamicLeftToRightColumnsLayoutTest >> testNullPresentersNeededFor [
	self
		assert: (layout nullPresentersNeededFor: presentersBuilder) equals: 0
]
