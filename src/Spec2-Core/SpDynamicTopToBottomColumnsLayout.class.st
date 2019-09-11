"
I am the layout for which you define the number of columns to store you presenters and set them from top to bottom in these rows.
"
Class {
	#name : #SpDynamicTopToBottomColumnsLayout,
	#superclass : #SpDynamicLeftToRightColumnsLayout,
	#category : #'Spec2-Core-Dynamic-Builder'
}

{ #category : #building }
SpDynamicTopToBottomColumnsLayout >> instVarNamesFrom: aDynamicPresentersListBuilder [
	| names streams |
	names := super instVarNamesFrom: aDynamicPresentersListBuilder.
	streams := names groupsOf: names size / self columnsCount atATimeCollect: [ :a | a readStream ].
	
	^ OrderedCollection streamContents: [ :s |
		[ streams noneSatisfy: #atEnd ]
			whileTrue: [ 
				streams do: [ :subS |
					s nextPut: subS next ] ] ]
]
