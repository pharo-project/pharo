"
I open the spec instances once the entire fixture and stimuli has run on it.
Then, assertions are run on an open presenter that should have been correctly configured.
"
Class {
	#name : #SpecAfterTestInitializationStrategy,
	#superclass : #SpecInitializationStrategy,
	#category : #'Spec-MorphicBackendTests'
}

{ #category : #running }
SpecAfterTestInitializationStrategy >> beforeAssertInTest: aSpecTest [
	
	"Assume openInstance will open it only the first time"
	aSpecTest openInstance.
]
