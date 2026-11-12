"
This class is an extension so we can mix command of Commander2 with Commander.
"
Class {
	#name : 'SycCmCommand',
	#superclass : 'ClyCommand',
	#traits : 'TRefactoringCommandSupport',
	#classTraits : 'TRefactoringCommandSupport classTrait',
	#instVars : [
		'parentGroup'
	],
	#category : 'SystemCommands-RefactoringSupport',
	#package : 'SystemCommands-RefactoringSupport'
}

{ #category : 'default' }
SycCmCommand class >> defaultDescription [
	"Answer a <String> describing the receiver, useful for instance in menu items pop-ups"

	^ self comment
]

{ #category : 'testing' }
SycCmCommand >> canBeExecuted [

	^ true
]

{ #category : 'accessing' }
SycCmCommand >> description [
	^ basicDescription
]

{ #category : 'executing' }
SycCmCommand >> executeRefactoring [
]

{ #category : 'testing' }
SycCmCommand >> isEnabled [

	^ true
]

{ #category : 'accessing' }
SycCmCommand >> name [
	^ basicName
]

{ #category : 'accessing' }
SycCmCommand >> order [

	^ 10100
]

{ #category : 'accessing' }
SycCmCommand >> parentGroup [

	^ parentGroup
]

{ #category : 'accessing' }
SycCmCommand >> parentGroup: aCmdRootMenuGroup [

	parentGroup := aCmdRootMenuGroup
]

{ #category : 'preparation' }
SycCmCommand >> prepareFullExecution [
]

{ #category : 'context menu' }
SycCmCommand >> registerContextMenuItemsWithBuilder: aBuilder [

	(aBuilder item: self name)
		parent: self parentGroup name;
		order: self parentGroup order + (self order / 100);
		help: self description;
		action: [self execute];
		icon: self icon
]
