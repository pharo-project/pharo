"
I open the spec instances before the stimuli has run on it.
Then, all stimuli is run on an open presenter that should been correctly configured.
"
Class {
	#name : #SpBeforeTestInitializationStrategy,
	#superclass : #SpInitializationStrategy,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpBeforeTestInitializationStrategy >> beforeTest: aSpecTest [

	aSpecTest openInstance
]
