"
I am a styler for tests that will add instances of SHRBTextFlagAttributes to the nodes instead of the normal style to ease the testing.
"
Class {
	#name : #SHRBMockTextStyler,
	#superclass : #SHRBTextStyler,
	#category : #'Shout-Tests-Styling'
}

{ #category : #private }
SHRBMockTextStyler >> attributesFor: aSymbol [

	^ { (SHRBTextFlagAttribute named: aSymbol) }
]
