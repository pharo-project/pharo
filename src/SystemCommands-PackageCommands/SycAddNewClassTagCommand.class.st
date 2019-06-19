"
I am a command to add new class tag into the package.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	package:		<RPackage>
	tagName:		<String>

"
Class {
	#name : #SycAddNewClassTagCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'package',
		'tagName'
	],
	#category : #'SystemCommands-PackageCommands'
}

{ #category : #testing }
SycAddNewClassTagCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isPackageSelected
]

{ #category : #execution }
SycAddNewClassTagCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.
	aToolContext showPackage: package atClassTag: tagName
]

{ #category : #accessing }
SycAddNewClassTagCommand >> defaultMenuIconName [
	^#add
]

{ #category : #accessing }
SycAddNewClassTagCommand >> defaultMenuItemName [
	^'New tag'
]

{ #category : #execution }
SycAddNewClassTagCommand >> execute [
	
	package addClassTag: tagName
]

{ #category : #accessing }
SycAddNewClassTagCommand >> package [
	^ package
]

{ #category : #accessing }
SycAddNewClassTagCommand >> package: anObject [
	package := anObject
]

{ #category : #execution }
SycAddNewClassTagCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	package := aToolContext lastSelectedPackage.
	
	tagName := UIManager default 
		request: ('New class tag in ' , package name) 
		initialAnswer: '' 
		title: 'Create a new class tag'.
	tagName isEmptyOrNil ifTrue: [ CmdCommandAborted signal ]
]

{ #category : #accessing }
SycAddNewClassTagCommand >> tagName [
	^ tagName
]

{ #category : #accessing }
SycAddNewClassTagCommand >> tagName: anObject [
	tagName := anObject
]
