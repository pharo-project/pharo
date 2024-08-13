"
Stub class to do not compute dependencies of the given package butrather give pre-established responses back.

#dependenciesOf will return  packages already in the bootsrap layer.
"
Class {
	#name : 'PharoBootstrapAnalyzerStubWithNoNewDependency',
	#superclass : 'Object',
	#category : 'Specific-Rules-Tests',
	#package : 'Specific-Rules-Tests'
}

{ #category : 'accessing' }
PharoBootstrapAnalyzerStubWithNoNewDependency >> dependenciesOf: aPackageName [
	^ #('Collections-Strings' Kernel 'AST-Core')
]
