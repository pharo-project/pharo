"
I open the spec instances before the stimuli has run on it.
Then, all stimuli is run on an open presenter that should been correctly configured.
"
Class {
	#name : #SpecBeforeTestInitializationStrategy,
	#superclass : #SpecInitializationStrategy,
	#category : #'Spec-BackendTests'
}

{ #category : #accessing }
SpecBeforeTestInitializationStrategy >> beforeTest: aSpecTest [

	aSpecTest openInstance
]
