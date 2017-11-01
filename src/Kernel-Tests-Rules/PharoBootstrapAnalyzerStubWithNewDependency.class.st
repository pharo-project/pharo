"
Stub class to do not compute dependencies of the given package butrather give pre-established responses back.
"
Class {
	#name : #PharoBootstrapAnalyzerStubWithNewDependency,
	#superclass : #Object,
	#category : 'Kernel-Tests-Rules'
}

{ #category : #accessing }
PharoBootstrapAnalyzerStubWithNewDependency >> dependenciesOf: aPackageName [
	^ #(Foo Bar)
]
