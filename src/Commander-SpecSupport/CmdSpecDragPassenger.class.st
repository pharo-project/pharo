Class {
	#name : #CmdSpecDragPassenger,
	#superclass : #Object,
	#instVars : [
		'sourcePresenter',
		'draggedItems',
		'commandPassenger'
	],
	#category : #'Commander-SpecSupport'
}

{ #category : #'instance creation' }
CmdSpecDragPassenger class >> from: sourcePresenter on: aDragPassenger with: draggedItems [

	^self new 
		sourcePresenter: sourcePresenter;
		commandPassenger: aDragPassenger;
		draggedItems: draggedItems

]

{ #category : #converting }
CmdSpecDragPassenger >> asDraggableMorph [
	^(draggedItems collect: [:each | sourcePresenter displayBlock value: each])
	 	asDraggableMorph
]

{ #category : #'*Commander-SpecSupport' }
CmdSpecDragPassenger >> buildWithSpec [
	^self
]

{ #category : #testing }
CmdSpecDragPassenger >> canBeDroppedInContext: aToolContext [

	^commandPassenger canBeDroppedInContext: aToolContext
]

{ #category : #accessing }
CmdSpecDragPassenger >> commandPassenger [
	^ commandPassenger
]

{ #category : #accessing }
CmdSpecDragPassenger >> commandPassenger: anObject [
	commandPassenger := anObject
]

{ #category : #accessing }
CmdSpecDragPassenger >> draggedItems [
	^ draggedItems
]

{ #category : #accessing }
CmdSpecDragPassenger >> draggedItems: anObject [
	draggedItems := anObject
]

{ #category : #dropping }
CmdSpecDragPassenger >> dropInContext: aToolContext [
	commandPassenger dropInContext: aToolContext
]

{ #category : #accessing }
CmdSpecDragPassenger >> sourcePresenter [
	^ sourcePresenter
]

{ #category : #accessing }
CmdSpecDragPassenger >> sourcePresenter: anObject [
	sourcePresenter := anObject
]
