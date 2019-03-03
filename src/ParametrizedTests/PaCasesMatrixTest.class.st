"
Testing class
"
Class {
	#name : #PaCasesMatrixTest,
	#superclass : #PaAbstractExampleTest,
	#category : #'ParametrizedTests-Tests'
}

{ #category : #private }
PaCasesMatrixTest class >> testParameters [

	^ ParametrizedTestMatrix new
		addCase: { #option1 -> 'a'. #option2 -> 1};
		addCase: { #option1 -> 'b'. #option2 -> 2};		
		addCase: { #option1 -> 'c'. #option2 -> 3};		
		yourself.
]
