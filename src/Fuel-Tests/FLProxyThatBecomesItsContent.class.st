"
Used at #testObjectByProxyThatBecomesItsContent
"
Class {
	#name : #FLProxyThatBecomesItsContent,
	#superclass : #Object,
	#instVars : [
		'contents'
	],
	#category : #'Fuel-Tests-Mocks'
}

{ #category : #'instance creation' }
FLProxyThatBecomesItsContent class >> newWith: aByteSymbol [ 
	
	^self basicNew
		initializeWith: aByteSymbol;
		yourself
]

{ #category : #serialization }
FLProxyThatBecomesItsContent >> fuelAfterMaterialization [ 

	self become: contents
]

{ #category : #initialization }
FLProxyThatBecomesItsContent >> initializeWith: anObject [
	
	super initialize.
	contents := anObject
]
