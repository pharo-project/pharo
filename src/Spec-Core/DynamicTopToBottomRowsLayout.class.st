"
I am the layout for which you define the number of rows to store you presenters and set them from top to bottom in these rows.
"
Class {
	#name : #DynamicTopToBottomRowsLayout,
	#superclass : #DynamicPresentersListLayout,
	#instVars : [
		'rowsCount'
	],
	#category : #'Spec-Core-Dynamic-Builder'
}

{ #category : #'instance creation' }
DynamicTopToBottomRowsLayout class >> rows: anInteger [
	^ self new
		rowsCount: anInteger;
		yourself
]

{ #category : #building }
DynamicTopToBottomRowsLayout >> buildLayoutFor: aDynamicPresentersListBuilder [
	^ SpecLayout composed
		newRow: [ :mainRow | 
			(self instVarNamesFrom: aDynamicPresentersListBuilder)
				groupsOf: self rowsCount
				atATimeDo: [ :group | 
					mainRow newColumn: [ :column |
						group do: [ :instVar | column newRow: instVar ] ] ] ]
]

{ #category : #building }
DynamicTopToBottomRowsLayout >> nullPresentersNeededFor: aDynamicPresentersListBuilder [
	"Compute the number of empty presenters required to fill additional space if the number of model objects modulo the number of columns/rows is not equals to 0."
	^ self rowsCount - ((aDynamicPresentersListBuilder modelObjects size - 1 % self rowsCount) + 1)
]

{ #category : #accessing }
DynamicTopToBottomRowsLayout >> rowsCount [
	^ rowsCount
]

{ #category : #accessing }
DynamicTopToBottomRowsLayout >> rowsCount: anObject [
	(anObject isInteger and: [ anObject > 0 ])
		ifFalse: [ self error: 'A positive integer is expected as parameter.' ].
	rowsCount := anObject
]
