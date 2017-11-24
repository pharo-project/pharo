"
A NaturalLanguageTranslator is a dummy translator.

The localization framework is found in the gettext package usually 
overriding this class completely. 

As an alternative you can register a translator using
  
   NaturalLanguageTranslator current: myTranslator

If this is done the messages will be dispatched to it
"
Class {
	#name : #NaturalLanguageTranslator,
	#superclass : #Object,
	#classVars : [
		'Current'
	],
	#category : #'System-Localization-Translations'
}

{ #category : #cleanup }
NaturalLanguageTranslator class >> cleanUp [
	 Current ifNotNil: [ Current cleanUp ]
]

{ #category : #accessing }
NaturalLanguageTranslator class >> current [ 
	"Return the currently registered translator"
	
	^Current 
]

{ #category : #accessing }
NaturalLanguageTranslator class >> current: aTranslator [
	"Register a translator that should receiver the translation messages"
	
	Current := aTranslator
]

{ #category : #translate }
NaturalLanguageTranslator class >> translate: aString [
	"Dispatch to a registered translator to translate the given string or (if there is no translator) return the untranslated string."
	Current ifNotNil: [ ^Current translate: aString ].
	^ aString
]

{ #category : #translate }
NaturalLanguageTranslator class >> translate: aString toLocale: localeID [
	"Dispatch to a registered translator to translate the given string with the given locale ID or (if there is no translator) return the untranslated string."
	Current ifNotNil: [ ^Current translate: aString toLocale: localeID ].
	^ aString
]
