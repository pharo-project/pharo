"
I am a class giving the user the hability to wait for an UI redraw in Morphic. 

I am useful for tests when they need to be executed in a CI in non interactive mode.
"
Trait {
	#name : #SpTMorphicUIRedrawer,
	#instVars : [
		'uiWaitingSemaphore'
	],
	#category : #'Spec2-Backend-Tests'
}

{ #category : #running }
SpTMorphicUIRedrawer >> defaultWaitDuration [
	^ 500 milliSecond
]

{ #category : #initialization }
SpTMorphicUIRedrawer >> initializeSemaphore [
	uiWaitingSemaphore := Semaphore new
]

{ #category : #testing }
SpTMorphicUIRedrawer >> isRunningInUIProcess [
	^ UIManager default uiProcess == Processor activeProcess
]

{ #category : #utilities }
SpTMorphicUIRedrawer >> waitUntilUIRedrawed [
	"I wait until the UI has been redrawn. 
	I take care of selecting how to do it. 
	If I am in the CI I should defer a semaphore signal. 
	If I am running in the UI process I can directly execute a doOneCycle on the World.
	If I am in the CI the tests and the UI run in different process. So I should not do a #doOneCycle.
	If I do it, I am in a race condition!"

	self isRunningInUIProcess
		ifTrue: [ self currentWorld doOneCycle.
			^ self ].

	self currentWorld defer: [ uiWaitingSemaphore ifNotNil: #signal ].
	uiWaitingSemaphore wait: self defaultWaitDuration
]
