"
I am a command to push up given methods
"
Class {
	#name : #SycPushUpMethodCommand,
	#superclass : #SycMethodCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #execution }
SycPushUpMethodCommand >> asRefactorings [

	^methods collect: [:method | 
		RBPullUpMethodRefactoring pullUp: {method selector} from: method origin]
]

{ #category : #accessing }
SycPushUpMethodCommand >> defaultMenuIconName [ 
	^ #up
]

{ #category : #accessing }
SycPushUpMethodCommand >> defaultMenuItemName [
	^'Push up'
]

{ #category : #execution }
SycPushUpMethodCommand >> execute [

	self executeRefactorings
]

{ #category : #testing }
SycPushUpMethodCommand >> isComplexRefactoring [
	^true
]
