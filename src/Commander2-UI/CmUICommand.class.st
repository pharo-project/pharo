"
I am a command decorator adding informations useful when for usage in context of user interface (UI).

Basically, I add:
- an #icon (#blank by default)
- a #shortcutKey (optional)
- a #displayStrategy to handle how to show the command if it is not executable on a context (default is to be disabled)
- a #positionStrategy to handle on which side presenter with left and right side I should be (default is left)

"
Class {
	#name : #CmUICommand,
	#superclass : #CmCommandDecorator,
	#instVars : [
		'iconProvider',
		'iconName',
		'shortcutKey',
		'displayStrategy',
		'positionStrategy'
	],
	#category : #'Commander2-UI-Core'
}

{ #category : #configuring }
CmUICommand >> beDisabledWhenCantBeRun [
	self displayStrategy: CmUIDisableWhenCantBeRun new
]

{ #category : #configuring }
CmUICommand >> beDisplayedOnLeftSide [
	positionStrategy := CmUILeftPositionStrategy new
]

{ #category : #configuring }
CmUICommand >> beDisplayedOnRightSide [
	positionStrategy := CmUIRightPositionStrategy new
]

{ #category : #configuring }
CmUICommand >> beHiddenWhenCantBeRun [
	self displayStrategy: CmUIHideWhenCantBeRun new
]

{ #category : #accessing }
CmUICommand >> displayStrategy [
	^ displayStrategy
]

{ #category : #accessing }
CmUICommand >> displayStrategy: anObject [
	displayStrategy := anObject
]

{ #category : #testing }
CmUICommand >> hasIcon [

	^ self iconName isNotNil
]

{ #category : #testing }
CmUICommand >> hasShortcutKey [

	^ shortcutKey isNotNil
]

{ #category : #accessing }
CmUICommand >> icon [
	self iconName ifNil: [ ^ nil ].
	
	^ self iconNamed: self iconName
]

{ #category : #accessing }
CmUICommand >> iconName [
	^ iconName
]

{ #category : #accessing }
CmUICommand >> iconName: aSymbol [ 
	iconName := aSymbol
]

{ #category : #configuring }
CmUICommand >> iconName: aString from: anIconProvider [ 
	self iconProvider: anIconProvider.
	self iconName: aString.
]

{ #category : #accessing }
CmUICommand >> iconNamed: aSymbol [
	^ self iconProvider iconNamed: aSymbol
]

{ #category : #accessing }
CmUICommand >> iconProvider [
	^ iconProvider ifNil: [ iconProvider := Smalltalk ui icons ]
]

{ #category : #accessing }
CmUICommand >> iconProvider: anObject [
	iconProvider := anObject
]

{ #category : #initialization }
CmUICommand >> initialize [ 

	super initialize.
	self beDisabledWhenCantBeRun.
	self beDisplayedOnLeftSide
]

{ #category : #accessing }
CmUICommand >> positionStrategy [
	^ positionStrategy
]

{ #category : #printing }
CmUICommand >> printOn: stream [ 

	super printOn: stream.
	stream << '(' << self name << ')'
]

{ #category : #accessing }
CmUICommand >> shortcutKey [
	^ shortcutKey ifNil: [ CmNoShortcutIsDefined signalCommand: self ]
]

{ #category : #accessing }
CmUICommand >> shortcutKey: aShortcutKey [
 	shortcutKey := aShortcutKey 
]
