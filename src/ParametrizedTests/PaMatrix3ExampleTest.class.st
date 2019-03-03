"
Testing class
"
Class {
	#name : #PaMatrix3ExampleTest,
	#superclass : #PaAbstractExampleTest,
	#instVars : [
		'option3'
	],
	#category : #'ParametrizedTests-Tests'
}

{ #category : #private }
PaMatrix3ExampleTest class >> testParameters [

	^ ParametrizedTestMatrix new
		forSelector: #option1 addOptions: #(a b c);
		forSelector: #option2 addOptions: {[1].[2].[3]};
		forSelector: #option3 addOptions: #(x y z);
		yourself.
]

{ #category : #accessing }
PaMatrix3ExampleTest >> option3 [
	^ option3
]

{ #category : #accessing }
PaMatrix3ExampleTest >> option3: anObject [
	option3 := anObject
]
