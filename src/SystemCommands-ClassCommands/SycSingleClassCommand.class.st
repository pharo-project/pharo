"
I am a base class for commands which perform operations with single class
 
Internal Representation and Key Implementation Points.

    Instance Variables
	targetClass:		<Class>
"
Class {
	#name : 'SycSingleClassCommand',
	#superclass : 'ClyCommand',
	#instVars : [
		'targetClass'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'class initialization' }
SycSingleClassCommand class >> initialize [
	"SycClassExtraCmCommand was the root of the commands of the Extra menu of classes. It is merged into me, and its name is kept so that the projects which subclass it still load. Their commands are not shown until they are declared with <calypsoCommands> in SycClassExtraMenuGroup. To remove in Pharo 16."

	self deprecatedAliases: #( #SycClassExtraCmCommand )
]

{ #category : 'testing' }
SycSingleClassCommand >> canBeExecuted [

	^ super canBeExecuted and: [ self context isClassSelected ]
]

{ #category : 'accessing' }
SycSingleClassCommand >> orderInContext: aToolContext [

	^ aToolContext isFullBrowserClassContext
		ifTrue: [ self fullBrowserOrder ]
		ifFalse: [ self order ]
]

{ #category : 'execution' }
SycSingleClassCommand >> prepareFullExecution [

	targetClass := self context lastSelectedClass
]

{ #category : 'accessing' }
SycSingleClassCommand >> targetClass [
	^ targetClass
]

{ #category : 'accessing' }
SycSingleClassCommand >> targetClass: anObject [
	targetClass := anObject
]
