"
I am a command to generate all unimplemented methods of the given classes
"
Class {
	#name : #SycRealizeClassCommand,
	#superclass : #SycClassCommand,
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #activation }
SycRealizeClassCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 2.1 for: ClyFullBrowserClassContext 
]

{ #category : #execution }
SycRealizeClassCommand >> asRefactorings [
	
	^classes collect: [ :each | RBRealizeClassRefactoring className: each]
]

{ #category : #accessing }
SycRealizeClassCommand >> defaultMenuItemName [
	^'Realize'
]

{ #category : #execution }
SycRealizeClassCommand >> execute [

	self executeRefactorings
]

{ #category : #testing }
SycRealizeClassCommand >> isComplexRefactoring [
	^true
]
