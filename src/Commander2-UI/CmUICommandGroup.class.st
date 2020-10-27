"
I am a command group decorator adding informations useful when for usage in context of user interface (UI).

Basically, I add:
- an #icon (#blank by default)
- the strategy to display commands group in a menu (I am displayed as a sub-menu by default).
"
Class {
	#name : #CmUICommandGroup,
	#superclass : #CmCommandGroupDecorator,
	#instVars : [
		'displayStrategy',
		'isRoot',
		'icon'
	],
	#category : #'Commander2-UI-Core'
}

{ #category : #default }
CmUICommandGroup class >> defaultDisplayStrategy [

	^ CmUIDisplayAsSubMenu new
]

{ #category : #default }
CmUICommandGroup class >> defaultIconName [
	^ #blank
]

{ #category : #configuring }
CmUICommandGroup >> beDisplayedAsGroup [
	self displayStrategy: CmUIDisplayAsGroup new
]

{ #category : #configuring }
CmUICommandGroup >> beDisplayedAsSubMenu [
	self displayStrategy: CmUIDisplayAsSubMenu new
]

{ #category : #configuring }
CmUICommandGroup >> beRoot [
	self isRoot: true.
]

{ #category : #accessing }
CmUICommandGroup >> displayStrategy [
	^ displayStrategy
]

{ #category : #accessing }
CmUICommandGroup >> displayStrategy: anObject [
	displayStrategy := anObject
]

{ #category : #accessing }
CmUICommandGroup >> icon [ 

	^ icon
]

{ #category : #accessing }
CmUICommandGroup >> icon: anIcon [

	icon := anIcon
]

{ #category : #accessing }
CmUICommandGroup >> iconName: aSymbol [


	self icon: (self iconNamed: aSymbol)
]

{ #category : #initialization }
CmUICommandGroup >> initialize [

	super initialize.
	self displayStrategy: self class defaultDisplayStrategy.
	self isRoot: false
]

{ #category : #accessing }
CmUICommandGroup >> isRoot [
	^ isRoot
]

{ #category : #accessing }
CmUICommandGroup >> isRoot: anObject [
	isRoot := anObject
]
