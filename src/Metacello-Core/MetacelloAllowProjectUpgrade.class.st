Class {
	#name : #MetacelloAllowProjectUpgrade,
	#superclass : #MetacelloResolveProjectUpgrade,
	#category : 'Metacello-Core-Scripts'
}

{ #category : #'exception handling' }
MetacelloAllowProjectUpgrade >> defaultAction [
    "Default for Upgrade is to allow"

    self checkAllowed.
    ^ self newProjectRegistration
]

{ #category : #handlers }
MetacelloAllowProjectUpgrade >> handleOnDownGrade: onDownGradeBlock onUpgrade: onUpgradeBlock [
    ^ onUpgradeBlock cull: self cull: self existingProjectRegistration cull: self newProjectRegistration
]

{ #category : #handlers }
MetacelloAllowProjectUpgrade >> handleResolutionFor: aScriptEngine [
    ^ aScriptEngine handleUpgrade: self
]

{ #category : #accessing }
MetacelloAllowProjectUpgrade >> operationString [
  ^ 'upgrade to'
]
