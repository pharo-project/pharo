"
The profile is a class that represents sets of configurations. 

For adding a new profile, we have to add a class method with the pragma <profile: #name>, this method should return a TKTProfile object with it configuration.

The profile building process ensures that the profile is properly setted from the debugging point of view. 

For setting the currently working profile see in the TKTConfiguration class.
"
Class {
	#name : #TKTProfile,
	#superclass : #Object,
	#instVars : [
		'name',
		'runner',
		'process',
		'poolWorkerProcess',
		'errorHandler',
		'processProvider',
		'serviceManager',
		'debugging',
		'watchDog'
	],
	#category : #'TaskIt-Profiles'
}

{ #category : #'accessing profiles' }
TKTProfile class >> defaultProfile [

	^ self defaultProfileClass new
]

{ #category : #'accessing profiles' }
TKTProfile class >> defaultProfileClass [

	^ (self allSubclasses sorted: [ :a | a priority ] descending) first
]

{ #category : #'accessing profiles' }
TKTProfile class >> defaultProfileName [

	^ self defaultProfileClass profileName
]

{ #category : #'instance creation' }
TKTProfile class >> new [

	^ self on: self profileValues
]

{ #category : #'instance creation' }
TKTProfile class >> on: aDictionary [
	^ self basicNew
		initialize;
		values: aDictionary;
		yourself
]

{ #category : #private }
TKTProfile class >> priority [

	^ self subclassResponsibility
]

{ #category : #'accessing profiles' }
TKTProfile class >> production [
	<profile: #production>

	^ TKTProductionProfile new
]

{ #category : #accessing }
TKTProfile class >> profileName [
	
	^ self subclassResponsibility
]

{ #category : #private }
TKTProfile class >> profileValues [

	^ self subclassResponsibility
]

{ #category : #accessing }
TKTProfile class >> profiles [
	^ (Pragma allNamed: #profile: from: self class to: ProtoObject) asOrderedCollection
		collect: [ :c | c arguments first -> (MessageSend receiver: self selector: c method selector) ]
]

{ #category : #accessing }
TKTProfile >> activateProfile [
	self debugging
		ifTrue: [ self options
				do: [ :option | self assert: (self at: option) isDebuggingCompatible description: ('The option{1} is not debugging compatible! ' format: {option}) ].
				(self at: #process) enableDebugger ]
		ifFalse: [ (self at: #process) disableDebugger ].
	self runner start
]

{ #category : #accessing }
TKTProfile >> at: anEntry [
	^ self instVarNamed: anEntry asSymbol
]

{ #category : #accessing }
TKTProfile >> at: anEntry put: aValue [
	^ self instVarNamed: anEntry asSymbol put: aValue
]

{ #category : #accessing }
TKTProfile >> debugging [
	^ debugging
]

{ #category : #accessing }
TKTProfile >> debugging: anObject [
	debugging := anObject
]

{ #category : #accessing }
TKTProfile >> errorHandler [
	^ errorHandler
]

{ #category : #accessing }
TKTProfile >> errorHandler: anObject [
	errorHandler := anObject
]

{ #category : #accessing }
TKTProfile >> finalizeProfile [
	self options do: [ :option | (self at: option) finalize ]
]

{ #category : #accessing }
TKTProfile >> name [
	^ name
]

{ #category : #accessing }
TKTProfile >> name: anObject [
	name := anObject
]

{ #category : #accessing }
TKTProfile >> options [
	^ #(runner process poolWorkerProcess errorHandler processProvider serviceManager)
]

{ #category : #accessing }
TKTProfile >> poolWorkerProcess [
	^ poolWorkerProcess
]

{ #category : #accessing }
TKTProfile >> poolWorkerProcess: anObject [
	poolWorkerProcess := anObject
]

{ #category : #accessing }
TKTProfile >> process [
	^ process
]

{ #category : #accessing }
TKTProfile >> process: anObject [
	process := anObject
]

{ #category : #accessing }
TKTProfile >> processProvider [
	^ processProvider
]

{ #category : #accessing }
TKTProfile >> processProvider: anObject [
	processProvider := anObject
]

{ #category : #accessing }
TKTProfile >> runner [
	^ runner
]

{ #category : #accessing }
TKTProfile >> runner: anObject [
	runner := anObject
]

{ #category : #accessing }
TKTProfile >> serviceManager [
	^ serviceManager
]

{ #category : #accessing }
TKTProfile >> serviceManager: anObject [
	serviceManager := anObject
]

{ #category : #accessing }
TKTProfile >> values: aDictionary [
	aDictionary keysAndValuesDo: [ :k :v | self perform: ('{1}:' format: {k}) asSymbol withArguments: {v} ]
]

{ #category : #accessing }
TKTProfile >> watchDog [
	^ watchDog
]

{ #category : #accessing }
TKTProfile >> watchDog: aWatchDog [
	watchDog := aWatchDog
]
