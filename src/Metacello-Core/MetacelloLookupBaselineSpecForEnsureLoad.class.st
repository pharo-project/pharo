Class {
	#name : #MetacelloLookupBaselineSpecForEnsureLoad,
	#superclass : #MetacelloScriptProjectSpecNotification,
	#category : #Metacello-Core-Scripts
}

{ #category : #'exception description' }
MetacelloLookupBaselineSpecForEnsureLoad >> defaultAction [
	"Answer <true> if you want to ensure the load of the baseline ... default is to load the baseline"

	^ true
]

{ #category : #handlers }
MetacelloLookupBaselineSpecForEnsureLoad >> handleResolutionFor: aScriptEngine [
	^ aScriptEngine handleLookupBaselineSpecForEnsureLoad: self
]
