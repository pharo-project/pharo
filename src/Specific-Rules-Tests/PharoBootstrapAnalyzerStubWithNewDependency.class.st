"
Stub class to do not compute dependencies of the given package butrather give pre-established responses back.
"
Class {
	#name : 'PharoBootstrapAnalyzerStubWithNewDependency',
	#superclass : 'Object',
	#category : 'Specific-Rules-Tests',
	#package : 'Specific-Rules-Tests'
}

{ #category : 'accessing' }
PharoBootstrapAnalyzerStubWithNewDependency >> dependenciesOf: aPackageName [
	^ #(Foo Bar)
]
