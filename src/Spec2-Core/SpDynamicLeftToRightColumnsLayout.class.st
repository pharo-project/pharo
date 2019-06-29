"
I am the layout for which you define the number of columns to store you presenters and set them from left to right in these rows.
"
Class {
	#name : #SpDynamicLeftToRightColumnsLayout,
	#superclass : #SpDynamicPresentersListLayout,
	#instVars : [
		'columnsCount'
	],
	#category : #'Spec2-Core-Dynamic-Builder'
}

{ #category : #'instance creation' }
SpDynamicLeftToRightColumnsLayout class >> columns: anInteger [
	^ self new
		columnsCount: anInteger;
		yourself
]

{ #category : #building }
SpDynamicLeftToRightColumnsLayout >> buildLayoutFor: aDynamicPresentersListBuilder [
	| layout |
	layout := SpBoxLayout newHorizontal.
	(self instVarNamesFrom: aDynamicPresentersListBuilder)
		groupsOf: self columnsCount
		atATimeDo: [ :group | 
			| subLayout |
			subLayout := SpBoxLayout newVertical.
			group do: [ :instVar | subLayout add: instVar ].
			layout add: subLayout ].

	^ layout
]

{ #category : #accessing }
SpDynamicLeftToRightColumnsLayout >> columnsCount [
	^ columnsCount
]

{ #category : #accessing }
SpDynamicLeftToRightColumnsLayout >> columnsCount: anObject [
	(anObject isInteger and: [ anObject > 0 ])
		ifFalse: [ self error: 'A positive integer is expected as parameter.' ].
	
	columnsCount := anObject
]

{ #category : #building }
SpDynamicLeftToRightColumnsLayout >> nullPresentersNeededFor: aDynamicPresentersListBuilder [
	"Compute the number of empty presenters required to fill additional space if the number of model objects modulo the number of columns/rows is not equals to 0."
	^ self columnsCount - (((aDynamicPresentersListBuilder modelObjects size - 1) % self columnsCount) + 1)
]
