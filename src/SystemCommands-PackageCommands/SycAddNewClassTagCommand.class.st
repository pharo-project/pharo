"
I am a command to add new class tag into the package.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	package:		<Package>
	tagName:		<String>

"
Class {
	#name : 'SycAddNewClassTagCommand',
	#superclass : 'SycPackageCommand',
	#instVars : [
		'package',
		'tagName'
	],
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'execution' }
SycAddNewClassTagCommand >> applyCommandResult [
	super applyCommandResult.
	self context showPackage: package tag: tagName
]

{ #category : 'execution' }
SycAddNewClassTagCommand >> executeRefactoring [

	package ensureTag: tagName
]

{ #category : 'accessing' }
SycAddNewClassTagCommand >> iconName [
	^#add
]

{ #category : 'accessing' }
SycAddNewClassTagCommand >> name [
	^'New tag'
]

{ #category : 'accessing' }
SycAddNewClassTagCommand >> order [

	^ 1000
]

{ #category : 'accessing' }
SycAddNewClassTagCommand >> package [
	^ package
]

{ #category : 'accessing' }
SycAddNewClassTagCommand >> package: anObject [
	package := anObject
]

{ #category : 'execution' }
SycAddNewClassTagCommand >> prepareFullExecution [

	super prepareFullExecution.
	package := self context lastSelectedPackage.
	tagName := self morphicUIManager request: 'New class tag in ' , package name initialAnswer: '' title: 'Create a new class tag'.
	tagName isEmptyOrNil ifTrue: [ CmCommandAborted signal ]
]

{ #category : 'accessing' }
SycAddNewClassTagCommand >> tagName [
	^ tagName
]

{ #category : 'accessing' }
SycAddNewClassTagCommand >> tagName: anObject [
	tagName := anObject
]
