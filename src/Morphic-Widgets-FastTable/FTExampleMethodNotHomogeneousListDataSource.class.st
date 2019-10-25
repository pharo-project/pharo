Class {
	#name : #FTExampleMethodNotHomogeneousListDataSource,
	#superclass : #FTExampleMethodListDataSource,
	#category : #'Morphic-Widgets-FastTable-Examples'
}

{ #category : #accessing }
FTExampleMethodNotHomogeneousListDataSource >> cellColumn: column row: rowIndex [
 
	^ FTCellMorph new
		cellInset: 5;
		addMorphBack: (self iconFor: rowIndex) asMorph;
		addMorphBack: (Morph new 
			changeTableLayout;
			hResizing: #shrinkWrap;
			listDirection: #topToBottom;
			color: Color transparent;
			addMorphBack: ((self elementAt: rowIndex) protocol asMorph emphasis: 2r10);
			addMorphBack: (self elementAt: rowIndex) name asMorph;
			yourself);
		yourself
]

{ #category : #accessing }
FTExampleMethodNotHomogeneousListDataSource >> rowHeight: rowIndex [

	^ 50
]
