"
I am a command to push down given methods
"
Class {
	#name : #SycPushDownMethodCommand,
	#superclass : #SycMethodCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #execution }
SycPushDownMethodCommand >> asRefactorings [

	^methods collect: [:method | 
		RBPushDownMethodRefactoring pushDown: {method selector} from: method origin]
]

{ #category : #accessing }
SycPushDownMethodCommand >> defaultMenuItemName [
	^'Push down'
]

{ #category : #execution }
SycPushDownMethodCommand >> execute [

	self executeRefactorings
]

{ #category : #testing }
SycPushDownMethodCommand >> isComplexRefactoring [
	^true
]
