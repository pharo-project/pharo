Class {
	#name : #SpecInitializationStrategy,
	#superclass : #Object,
	#category : #'Spec-BackendTests'
}

{ #category : #accessing }
SpecInitializationStrategy class >> afterTest [
	
	^ SpecAfterTestInitializationStrategy new
]

{ #category : #accessing }
SpecInitializationStrategy class >> beforeTest [
	
	^ SpecBeforeTestInitializationStrategy new
]

{ #category : #running }
SpecInitializationStrategy >> beforeAssertInTest: aSpecTest [

	"By default do nothing.
	Hook for subclasses"
]

{ #category : #running }
SpecInitializationStrategy >> beforeTest: aSpecTest [ 
	
	"By default do nothing.
	Hook for subclasses"
]
