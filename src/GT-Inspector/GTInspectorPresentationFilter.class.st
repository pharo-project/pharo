"
A GTInspectorPresentationFilter is the root class for filters that can limit the number if visible presentations for an object.

Subclasses need to implement  #shouldDisplayPresentationCreateBy: to indicate whether or not a give presentation should be displayed.

"
Class {
	#name : #GTInspectorPresentationFilter,
	#superclass : #Object,
	#category : #'GT-Inspector'
}

{ #category : #accessing }
GTInspectorPresentationFilter >> changeFilterRequestFor: aGTInspector [

	self subclassResponsibility
]

{ #category : #private }
GTInspectorPresentationFilter >> openListChooseLabeled: sString forInspector: aGTInspector with: allElements select: selectedElements format: aBlockOrSelector actionSelector: aSymbol [
	|treeModel|
	
	treeModel := MorphTreeModel new.
	treeModel
		 beCheckList;
		 rootItems: allElements;
		 wrapBlockOrSelector: aBlockOrSelector;
		 autoMultiSelection: true.
	 (treeModel openDialogWindowIn: World title: sString selectedtems: selectedElements) ifNotNil: [ :selection | 
		self perform: aSymbol with: selection with: aGTInspector ]
]

{ #category : #testing }
GTInspectorPresentationFilter >> shouldDisplayPresentationCreatedBy: aMethod [

	self subclassResponsibility
]
