"
I am the layout for which you define the number of rows to store you presenters and set them from top to bottom in these rows.
"
Class {
	#name : #SpDynamicTopToBottomRowsLayout,
	#superclass : #SpDynamicPresentersListLayout,
	#instVars : [
		'rowsCount'
	],
	#category : #'Spec2-Core-Dynamic-Builder'
}

{ #category : #'instance creation' }
SpDynamicTopToBottomRowsLayout class >> rows: anInteger [
	^ self new
		rowsCount: anInteger;
		yourself
]

{ #category : #building }
SpDynamicTopToBottomRowsLayout >> buildLayoutFor: aDynamicPresentersListBuilder [
	| layout |
	layout := SpBoxLayout newVertical.
	(self instVarNamesFrom: aDynamicPresentersListBuilder)
		groupsOf: self rowsCount
		atATimeDo: [ :group | 
			| subLayout |
			subLayout := SpBoxLayout newHorizontal.
			group do: [ :instVar | subLayout add: instVar ].
			layout add: subLayout ].

	^ layout
]

{ #category : #building }
SpDynamicTopToBottomRowsLayout >> nullPresentersNeededFor: aDynamicPresentersListBuilder [
	"Compute the number of empty presenters required to fill additional space if the number of model objects modulo the number of columns/rows is not equals to 0."
	^ self rowsCount - ((aDynamicPresentersListBuilder modelObjects size - 1 % self rowsCount) + 1)
]

{ #category : #accessing }
SpDynamicTopToBottomRowsLayout >> rowsCount [
	^ rowsCount
]

{ #category : #accessing }
SpDynamicTopToBottomRowsLayout >> rowsCount: anObject [
	(anObject isInteger and: [ anObject > 0 ])
		ifFalse: [ self error: 'A positive integer is expected as parameter.' ].
	rowsCount := anObject
]
