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

	^classes collect: [ :each | self refactoringClass className: each name]
]

{ #category : 'accessing' }
SycRealizeClassCommand >> fullBrowserOrder [

	^ 2.1
]

{ #category : 'testing' }
SycRealizeClassCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserClassContext
]

{ #category : 'testing' }
SycRealizeClassCommand >> isComplexRefactoring [
	^true
]

{ #category : 'accessing' }
SycRealizeClassCommand >> name [
	"Make sure that the user knows that this is a transformation by adding (T) in front of the menu item name."

	^ self refactoringClass menuItemString
]

{ #category : 'accessing' }
SycRealizeClassCommand >> order [

	^ 2.1
]

{ #category : 'factory method' }
SycRealizeClassCommand >> refactoringClass [

	^ RBRealizeClassTransformation
]
