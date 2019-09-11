Class {
	#name : #AbstractMorphicUITest,
	#superclass : #TestCase,
	#instVars : [
		'uiWaitingSemaphore'
	],
	#category : #'Glamour-Tests-Morphic'
}

{ #category : #running }
AbstractMorphicUITest >> defaultWaitDuration [

	^ 500 milliSecond
]

{ #category : #running }
AbstractMorphicUITest >> existsUIProcess [

	^ UIManager default uiProcess isNotNil
]

{ #category : #running }
AbstractMorphicUITest >> isRunningInUIProcess [

	^ UIManager default uiProcess == Processor activeProcess
]

{ #category : #running }
AbstractMorphicUITest >> setUp [
	super setUp.
	uiWaitingSemaphore := Semaphore new
]

{ #category : #running }
AbstractMorphicUITest >> waitUntilUIRedrawed [

	"I wait until the UI has been redrawn. 
	I take care of selecting how to do it. 
	If I am in the CI I should defer a semaphore signal. 
	If I am running in the UI process I can directly execute a doOneCycle on the World.
	If I am in the CI the tests and the UI run in different process. So I should not do a #doOneCycle.
	If I do it, I am in a race condition!"
	self isRunningInUIProcess ifTrue: [ 
		self currentWorld doOneCycle.
		^ self.	
	]. 

	self currentWorld defer: [ uiWaitingSemaphore ifNotNil: #signal ].	
	uiWaitingSemaphore wait: self defaultWaitDuration.
]
