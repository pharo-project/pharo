Class {
	#name : #MetacelloResolveProjectUpgrade,
	#superclass : #MetacelloScriptNotification,
	#instVars : [
		'existingProjectRegistration',
		'newProjectRegistration'
	],
	#category : #Metacello-Core-Scripts
}

{ #category : #'exception handling' }
MetacelloResolveProjectUpgrade >> allow [
    self checkAllowed.
    self resume: self newProjectRegistration
]

{ #category : #'exception handling' }
MetacelloResolveProjectUpgrade >> allowEvenIfLocked [
  "for internal ScriptEngine use only. from the Scripting API use:
		onLocked: [:ex | ex allow]
		"

  self resume: self newProjectRegistration
]

{ #category : #private }
MetacelloResolveProjectUpgrade >> checkAllowed [
  self existingProjectRegistration locked
    ifTrue: [ 
      self
        resume:
          (MetacelloAllowLockedProjectChange new
            operationString: self operationString;
            existingProjectRegistration: self existingProjectRegistration;
            newProjectRegistration: self newProjectRegistration;
            signal) ]
]

{ #category : #'exception handling' }
MetacelloResolveProjectUpgrade >> defaultAction [
    "Result of signal should be the projectRegistration to be used to perform the load ... default is to disallow"

    ^ self existingProjectRegistration
]

{ #category : #'exception handling' }
MetacelloResolveProjectUpgrade >> disallow [
    "default action"

    self resume: self existingProjectRegistration
]

{ #category : #accessing }
MetacelloResolveProjectUpgrade >> existingProjectRegistration [
	^ existingProjectRegistration
]

{ #category : #accessing }
MetacelloResolveProjectUpgrade >> existingProjectRegistration: anObject [
	existingProjectRegistration := anObject
]

{ #category : #handlers }
MetacelloResolveProjectUpgrade >> handleOnDownGrade: onDownGradeBlock onUpgrade: onUpgradeBlock [
    self subclassResponsibility
]

{ #category : #accessing }
MetacelloResolveProjectUpgrade >> newProjectRegistration [
	^ newProjectRegistration
]

{ #category : #accessing }
MetacelloResolveProjectUpgrade >> newProjectRegistration: anObject [
	newProjectRegistration := anObject
]

{ #category : #accessing }
MetacelloResolveProjectUpgrade >> operationString [
    self subclassResponsibility
]

{ #category : #'exception handling' }
MetacelloResolveProjectUpgrade >> useExisting [
  "to be deprecated: useLoaded"

  self disallow
]

{ #category : #'exception handling' }
MetacelloResolveProjectUpgrade >> useIncoming [
  self allow
]

{ #category : #'exception handling' }
MetacelloResolveProjectUpgrade >> useLoaded [
  self disallow
]

{ #category : #'exception handling' }
MetacelloResolveProjectUpgrade >> useNew [
  "to be deprecated: useIncoming"

  self allow
]
