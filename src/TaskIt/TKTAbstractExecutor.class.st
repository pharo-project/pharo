"
I am an abstract class defining the basic behaviour and state to execute a task:
  - the basic executeTask: method
  - an exception handler meant to manage unhandled exception
"
Class {
	#name : #TKTAbstractExecutor,
	#superclass : #Object,
	#traits : 'TTaskExecutor',
	#classTraits : 'TTaskExecutor classTrait',
	#instVars : [
		'exceptionHandler',
		'busy'
	],
	#category : #'TaskIt-Kernel'
}

{ #category : #accessing }
TKTAbstractExecutor >> exceptionHandler [
	^ exceptionHandler
]

{ #category : #accessing }
TKTAbstractExecutor >> exceptionHandler: aHandler [
	
	exceptionHandler := aHandler
]

{ #category : #initialization }
TKTAbstractExecutor >> initialize [
	super initialize.
	busy := false.
	exceptionHandler := TKTConfiguration errorHandler
]

{ #category : #'as yet unclassified' }
TKTAbstractExecutor >> isBusy [

	^ busy
]

{ #category : #'as yet unclassified' }
TKTAbstractExecutor >> isFree [

	^ busy not
]

{ #category : #accessing }
TKTAbstractExecutor >> noteBusy [

	busy := true
]

{ #category : #accessing }
TKTAbstractExecutor >> noteFree [

	busy := false
]

{ #category : #polimorfism }
TKTAbstractExecutor >> start [
 " nothing to do "
]

{ #category : #polimorfsms }
TKTAbstractExecutor >> stop [

]
