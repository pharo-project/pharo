"
Testing class
"
Class {
	#name : #PaCombinedMatrixExampleTest,
	#superclass : #PaAbstractExampleTest,
	#instVars : [
		'option3'
	],
	#category : #'ParametrizedTests-Tests'
}

{ #category : #'building suites' }
PaCombinedMatrixExampleTest class >> testParameters [

	^ ParametrizedTestMatrix new
		addCase: { #option1 -> 'a'. #option2 -> 1};
		addCase: { #option1 -> 'b'. #option2 -> 2};		
		addCase: { #option1 -> 'c'. #option2 -> 3};		
		forSelector: #option3 addOptions: #(a b);
		yourself.
]

{ #category : #accessing }
PaCombinedMatrixExampleTest >> option3 [
	^ option3
]

{ #category : #accessing }
PaCombinedMatrixExampleTest >> option3: aString [ 
	option3 := aString
]
