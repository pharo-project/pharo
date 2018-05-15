"
Text converter for Simplified Chinese variation of EUC.  (Even though the name doesn't look so, it is what it is.)
"
Class {
	#name : #CNGBTextConverter,
	#superclass : #EUCTextConverter,
	#category : #'Multilingual-OtherLanguages-TextConverter'
}

{ #category : #utilities }
CNGBTextConverter class >> encodingNames [ 

	^ #('gb2312' ) copy

]

{ #category : #private }
CNGBTextConverter >> languageEnvironment [

	^ SimplifiedChineseEnvironment.

]

{ #category : #private }
CNGBTextConverter >> leadingChar [

	^ GB2312 leadingChar

]
