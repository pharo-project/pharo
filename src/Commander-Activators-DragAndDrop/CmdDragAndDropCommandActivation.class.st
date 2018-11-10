Class {
	#name : #CmdDragAndDropCommandActivation,
	#superclass : #CmdDragAndDropActivation,
	#category : #'Commander-Activators-DragAndDrop'
}

{ #category : #deprecation }
CmdDragAndDropCommandActivation class >> deprecationRefactorings [

	^ { RBApplyClassDeprecationRefactoring rename: self to: #CmdDragAndDropActivation}

]

{ #category : #deprecation }
CmdDragAndDropCommandActivation class >> isDeprecated [
	^true
]

{ #category : #deprecation }
CmdDragAndDropCommandActivation class >> new [
	^CmdDragAndDropActivation new
]
