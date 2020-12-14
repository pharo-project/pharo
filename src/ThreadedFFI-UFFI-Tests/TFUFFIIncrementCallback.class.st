Class {
	#name : #TFUFFIIncrementCallback,
	#superclass : #TFUFFIAbstractCallback,
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #initialization }
TFUFFIIncrementCallback >> initialize [ 
	
	self signature: #(int (int)) block: [ :a | a + 1 ]
]
