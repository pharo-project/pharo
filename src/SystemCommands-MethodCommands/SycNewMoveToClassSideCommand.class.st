"
I am a command to move method to the class side of defining class
"
Class {
	#name : #SycNewMoveToClassSideCommand,
	#superclass : #SycMethodCmCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #accessing }
SycNewMoveToClassSideCommand >> execute [
	
	self methods 
		collect: [ :each | RBMoveMethodToClassSideRefactoring method: each class: each origin ]
		thenDo: [ :each | each execute ]
]

{ #category : #accessing }
SycNewMoveToClassSideCommand >> icon [ 
	^ self iconNamed: #smallRedo
]

{ #category : #accessing }
SycNewMoveToClassSideCommand >> isEnabled [

	^ context isInstanceSideMethodSelected
]

{ #category : #accessing }
SycNewMoveToClassSideCommand >> methods [
	^ context selectedMethods
]

{ #category : #accessing }
SycNewMoveToClassSideCommand >> name [ 
	^ 'New move to class side'
]
