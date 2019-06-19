"
I am a command to apply class deprecation in system.
I ask class itself to get target refactorings and then execute them
"
Class {
	#name : #ClyApplyClassDeprecationCommand,
	#superclass : #SycSingleClassCommand,
	#category : #'Calypso-SystemPlugins-Deprecation-Browser'
}

{ #category : #testing }
ClyApplyClassDeprecationCommand class >> canBeExecutedInContext: aToolContext [
	^(super canBeExecutedInContext: aToolContext)
		and: [ aToolContext selectedClassItems last isMarkedWith: ClyDeprecatedItemTag  ]
]

{ #category : #activation }
ClyApplyClassDeprecationCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemFor: ClyClass asCalypsoItemContext
]

{ #category : #execution }
ClyApplyClassDeprecationCommand >> asRefactorings [

	^targetClass deprecationRefactorings
]

{ #category : #accessing }
ClyApplyClassDeprecationCommand >> defaultMenuItemName [
	^'Apply deprecation'
]

{ #category : #execution }
ClyApplyClassDeprecationCommand >> execute [
	
	self executeRefactorings
]

{ #category : #testing }
ClyApplyClassDeprecationCommand >> isComplexRefactoring [
	^true
]
