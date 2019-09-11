"
I am the layout for which you define the number of rows to store you presenters and set them from left to right in these rows.
"
Class {
	#name : #SpDynamicLeftToRightRowsLayout,
	#superclass : #SpDynamicTopToBottomRowsLayout,
	#category : #'Spec2-Core-Dynamic-Builder'
}

{ #category : #building }
SpDynamicLeftToRightRowsLayout >> instVarNamesFrom: aDynamicPresentersListBuilder [
	| names streams |
	names := super instVarNamesFrom: aDynamicPresentersListBuilder.
	streams := names groupsOf: names size / self rowsCount atATimeCollect: [ :a | a readStream ].
	
	^ OrderedCollection streamContents: [ :s |
		[ streams noneSatisfy: #atEnd ]
			whileTrue: [ 
				streams do: [ :subS |
					s nextPut: subS next ] ] ]
]
