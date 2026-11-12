"
I am a command to show local method version.

Local version has not relation to actual source code system. Versions are retrieved form changes file.

Internal Representation and Key Implementation Points.

    Instance Variables
	method:		<CompiledMethod>
"
Class {
	#name : 'SycShowMethodVersionCommand',
	#superclass : 'SycMethodCommand',
	#instVars : [
		'method'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'accessing' }
SycShowMethodVersionCommand >> description [

	^ 'Show local method version.'
]

{ #category : 'execution' }
SycShowMethodVersionCommand >> executeRefactoring [

	(Smalltalk tools toolNamed: #versionBrowser) browseVersionsForMethod: method
]

{ #category : 'accessing' }
SycShowMethodVersionCommand >> iconName [
	^#history
]

{ #category : 'accessing' }
SycShowMethodVersionCommand >> name [
	^'Versions'
]

{ #category : 'accessing' }
SycShowMethodVersionCommand >> order [

	^ 1000
]

{ #category : 'execution' }
SycShowMethodVersionCommand >> prepareFullExecution [

	super prepareFullExecution.
	method := self context lastSelectedMethod
]
