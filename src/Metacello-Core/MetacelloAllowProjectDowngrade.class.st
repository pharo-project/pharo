Class {
	#name : #MetacelloAllowProjectDowngrade,
	#superclass : #MetacelloResolveProjectUpgrade,
	#category : #Metacello-Core-Scripts
}

{ #category : #'exception handling' }
MetacelloAllowProjectDowngrade >> defaultAction [
  "Default for Downgrade is to disallow, but still should check for locks"

  self checkAllowed.
  ^ super defaultAction
]

{ #category : #handlers }
MetacelloAllowProjectDowngrade >> handleOnDownGrade: onDownGradeBlock onUpgrade: onUpgradeBlock [
    ^ onDownGradeBlock cull: self cull: self existingProjectRegistration cull: self newProjectRegistration
]

{ #category : #handlers }
MetacelloAllowProjectDowngrade >> handleResolutionFor: aScriptEngine [
    ^ aScriptEngine handleDowngrade: self
]

{ #category : #accessing }
MetacelloAllowProjectDowngrade >> operationString [
  ^ 'downgrade to'
]
