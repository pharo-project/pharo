"
EncodedCharSet initialize.
LanguageEnvironment resetKnownEnvironments.
Locale switchToID: (LocaleID isoLanguage: 'ru').

"
Class {
	#name : #RussianEnvironment,
	#superclass : #LanguageEnvironment,
	#category : #'Multilingual-OtherLanguages-Environments'
}

{ #category : #'class initialization' }
RussianEnvironment class >> initialize [

	EncodedCharSet declareEncodedCharSet: self atIndex: 15+1.
]

{ #category : #'subclass responsibilities' }
RussianEnvironment class >> leadingChar [

	^0
]

{ #category : #'subclass responsibilities' }
RussianEnvironment class >> supportedLanguages [
	"Return the languages that this class supports. 
	Any translations for those languages will use this class as their environment."
	
	^#('ru' )
]

{ #category : #'subclass responsibilities' }
RussianEnvironment class >> systemConverterClass [

	^ UTF8TextConverter
]
