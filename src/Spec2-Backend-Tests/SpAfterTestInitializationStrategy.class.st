"
I open the spec instances once the entire fixture and stimuli has run on it.
Then, assertions are run on an open presenter that should have been correctly configured.
"
Class {
	#name : #SpAfterTestInitializationStrategy,
	#superclass : #SpInitializationStrategy,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #running }
SpAfterTestInitializationStrategy >> beforeAssertInTest: aSpecTest [
	
	"Assume openInstance will open it only the first time"
	aSpecTest openInstance.
]
