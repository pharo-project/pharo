"
I am a simple callback used by the tests
"
Class {
	#name : 'TFUFFIIncrementCallback',
	#superclass : 'TFUFFIAbstractCallback',
	#category : 'UnifiedFFI-Tests-Tests',
	#package : 'UnifiedFFI-Tests',
	#tag : 'Tests'
}

{ #category : 'initialization' }
TFUFFIIncrementCallback >> initialize [

	self signature: #(int (int)) block: [ :a | a + 1 ]
]
