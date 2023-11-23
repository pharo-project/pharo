"
I am a command to generate all unimplemented methods of the given classes
"
Class {
	#name : 'SycRealizeClassCommand',
	#superclass : 'SycClassCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'execution' }
SycRealizeClassCommand >> asRefactorings [

	^classes collect: [ :each | self refactoringClass className: each]
]

{ #category : 'accessing' }
SycRealizeClassCommand >> defaultMenuItemName [
	"Make sure that the user knows that this is a transformation by adding (T) in front of the menu item name."

	^ self refactoringClass menuItemString
]

{ #category : 'execution' }
SycRealizeClassCommand >> execute [

	self executeRefactorings
]

{ #category : 'testing' }
SycRealizeClassCommand >> isComplexRefactoring [
	^true
]

{ #category : 'factory method' }
SycRealizeClassCommand >> refactoringClass [

	^ RBRealizeClassTransformation
]
