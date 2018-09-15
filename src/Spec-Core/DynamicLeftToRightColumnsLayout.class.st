"
I am the layout for which you define the number of columns to store you presenters and set them from left to right in these rows.
"
Class {
	#name : #DynamicLeftToRightColumnsLayout,
	#superclass : #DynamicPresentersListLayout,
	#instVars : [
		'columnsCount'
	],
	#category : #'Spec-Core-Dynamic-Builder'
}

{ #category : #'instance creation' }
DynamicLeftToRightColumnsLayout class >> columns: anInteger [
	^ self new
		columnsCount: anInteger;
		yourself
]

{ #category : #building }
DynamicLeftToRightColumnsLayout >> buildLayoutFor: aDynamicPresentersListBuilder [
	^ SpecLayout composed
		newColumn: [ :mainColumn | 
			(self instVarNamesFrom: aDynamicPresentersListBuilder)
				groupsOf: self columnsCount
				atATimeDo: [ :group | 
					mainColumn
						newRow: [ :row | group do: [ :instVar | row newColumn: instVar ] ] ] ]
]

{ #category : #accessing }
DynamicLeftToRightColumnsLayout >> columnsCount [
	^ columnsCount
]

{ #category : #accessing }
DynamicLeftToRightColumnsLayout >> columnsCount: anObject [
	(anObject isInteger and: [ anObject > 0 ])
		ifFalse: [ self error: 'A positive integer is expected as parameter.' ].
	
	columnsCount := anObject
]

{ #category : #building }
DynamicLeftToRightColumnsLayout >> nullPresentersNeededFor: aDynamicPresentersListBuilder [
	"Compute the number of empty presenters required to fill additional space if the number of model objects modulo the number of columns/rows is not equals to 0."
	^ self columnsCount - (((aDynamicPresentersListBuilder modelObjects size - 1) % self columnsCount) + 1)
]
