"
Usage: perform <global> <messageSelector>  [ <arguments> ] [ --save ]
	global	A class name or a global name. Receiver of the message (e.g. Smalltalk)
	messageSelector   The message selector to be perfomed
	
Documentation:
Performs a message on a given class or global object. This command-line handler can be used to execute some code if a compiler is not loaded in the image. All message arguments are strings.

Examples:
	pharo Pharo.image perform Smalltalk garbageCollect --save
	pharo Pharo.image perform PerformMessageCommandLineHandler printReversed:times:  olleH 10

"
Class {
	#name : #PerformMessageCommandLineHandler,
	#superclass : #CommandLineHandler,
	#category : #PerformMessageCommandLineHandler
}

{ #category : #testing }
PerformMessageCommandLineHandler class >> answerUltimateQuestion [

	Smalltalk exit: 42.
]

{ #category : #accessing }
PerformMessageCommandLineHandler class >> commandName [
	^ 'perform'
]

{ #category : #accessing }
PerformMessageCommandLineHandler class >> description [
	^ 'Perform given message on a specified class or global'
]

{ #category : #testing }
PerformMessageCommandLineHandler class >> printReversed: aString times: aStringWithNumber [

	aStringWithNumber asInteger timesRepeat: [ 
		Stdio stdout nextPutAll: aString reversed; newLine ]
]

{ #category : #activation }
PerformMessageCommandLineHandler >> activate [
	| aClass aMessage arguments messageArguments |
	self activateHelp
		ifTrue: [ ^ self ].
		
	arguments := self commandLine arguments reject: [ :argument | argument beginsWith: '-']. "ignore options"
	
	(arguments size < 2)
		ifTrue: [ ^ self exitFailure: 'Not enough arguments' ].
		
	aClass := Smalltalk globals at: arguments first asSymbol ifAbsent: [ 
		  ^ self exitFailure: 'Unknown class' ].
	aMessage := arguments second asSymbol.
	messageArguments := (arguments allButFirst: 2) asArray.
	
	aClass perform: aMessage withArguments: messageArguments.
	
	(self hasOption: 'save')
		ifTrue: [ Smalltalk snapshot: true andQuit: false ].
	self exitSuccess
]
