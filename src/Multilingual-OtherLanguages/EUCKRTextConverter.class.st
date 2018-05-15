"
Text converter for Korean variation of EUC.
"
Class {
	#name : #EUCKRTextConverter,
	#superclass : #EUCTextConverter,
	#category : #'Multilingual-OtherLanguages-TextConverter'
}

{ #category : #utilities }
EUCKRTextConverter class >> encodingNames [ 

	^ #('euc-kr' 'ks-c-5601-1987' 'euckr') copy

]

{ #category : #private }
EUCKRTextConverter >> languageEnvironment [

	^ KoreanEnvironment.

]

{ #category : #private }
EUCKRTextConverter >> leadingChar [

	^ KSX1001 leadingChar

]
