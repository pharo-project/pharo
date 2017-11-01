Class {
	#name : #MetacelloAllowLockedProjectChange,
	#superclass : #MetacelloResolveProjectUpgrade,
	#instVars : [
		'operationString'
	],
	#category : 'Metacello-Core-Scripts'
}

{ #category : #'exception handling' }
MetacelloAllowLockedProjectChange >> break [
  ^ self allow
]

{ #category : #private }
MetacelloAllowLockedProjectChange >> checkAllowed [
  "noop ... if user decided to allow lock to be broken, then so be it"
]

{ #category : #'exception handling' }
MetacelloAllowLockedProjectChange >> defaultAction [
  Warning
    signal:
      'LOCK ENFORCED: Attempt to ' , self operationString printString
        , ' new project: ' , self newProjectRegistration printString printString
        , ' when existing project: '
        , self existingProjectRegistration printString printString
        , ' is locked. New project not loaded. Use #onLock: to intercept.'.
  ^ self disallow
]

{ #category : #handlers }
MetacelloAllowLockedProjectChange >> handleResolutionFor: aScriptEngine [
  ^ aScriptEngine handleLock: self
]

{ #category : #'exception handling' }
MetacelloAllowLockedProjectChange >> honor [
  ^ self disallow
]

{ #category : #accessing }
MetacelloAllowLockedProjectChange >> operationString [
  ^ operationString
]

{ #category : #accessing }
MetacelloAllowLockedProjectChange >> operationString: aString [
  operationString := aString
]
