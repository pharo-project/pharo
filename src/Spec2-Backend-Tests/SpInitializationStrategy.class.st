Class {
	#name : #SpInitializationStrategy,
	#superclass : #Object,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpInitializationStrategy class >> afterTest [
	
	^ SpAfterTestInitializationStrategy new
]

{ #category : #accessing }
SpInitializationStrategy class >> beforeTest [
	
	^ SpBeforeTestInitializationStrategy new
]

{ #category : #running }
SpInitializationStrategy >> beforeAssertInTest: aSpecTest [

	"By default do nothing.
	Hook for subclasses"
]

{ #category : #running }
SpInitializationStrategy >> beforeTest: aSpecTest [ 
	
	"By default do nothing.
	Hook for subclasses"
]
