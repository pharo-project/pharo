"
I am the layout for which you define the number of rows to store you presenters and set them from left to right in these rows.
"
Class {
	#name : #DynamicLeftToRightRowsLayout,
	#superclass : #DynamicTopToBottomRowsLayout,
	#category : #'Spec-Core-Dynamic-Builder'
}

{ #category : #building }
DynamicLeftToRightRowsLayout >> instVarNamesFrom: aDynamicPresentersListBuilder [
	| names streams |
	names := super instVarNamesFrom: aDynamicPresentersListBuilder.
	streams := names groupsOf: names size / self rowsCount atATimeCollect: [ :a | a readStream ].
	
	^ OrderedCollection streamContents: [ :s |
		[ streams noneSatisfy: #atEnd ]
			whileTrue: [ 
				streams do: [ :subS |
					s nextPut: subS next ] ] ]
]
