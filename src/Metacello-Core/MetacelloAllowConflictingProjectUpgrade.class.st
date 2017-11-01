Class {
	#name : #MetacelloAllowConflictingProjectUpgrade,
	#superclass : #MetacelloResolveProjectUpgrade,
	#category : 'Metacello-Core-Scripts'
}

{ #category : #'exception handling' }
MetacelloAllowConflictingProjectUpgrade >> defaultAction [
  self existingProjectRegistration locked
    ifTrue: [ 
      "https://github.com/dalehenrich/metacello-work/issues/263"
      ^ self useIncoming ].
  ^ MetacelloConflictingProjectError
    signal:
      'Load Conflict between existing ' , self existingProjectRegistration printString
        , ' and ' , self newProjectRegistration printString
]

{ #category : #handlers }
MetacelloAllowConflictingProjectUpgrade >> handleResolutionFor: aScriptEngine [
    ^ aScriptEngine handleConflict: self
]

{ #category : #accessing }
MetacelloAllowConflictingProjectUpgrade >> operationString [
  ^ 'load with conflicts'
]
