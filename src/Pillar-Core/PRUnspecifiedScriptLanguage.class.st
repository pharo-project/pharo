"
An unspecified script language
"
Class {
	#name : #PRUnspecifiedScriptLanguage,
	#superclass : #PRAbstractScriptLanguage,
	#category : #'Pillar-Core-Model'
}

{ #category : #protected }
PRUnspecifiedScriptLanguage class >> acceptLanguage: aLanguageName [
	^ aLanguageName isNil or: [ super acceptLanguage: aLanguageName ]
]

{ #category : #testing }
PRUnspecifiedScriptLanguage class >> isAbstract [
	^ false
]

{ #category : #protected }
PRUnspecifiedScriptLanguage class >> standardName [
	^ 'unspecified language'
]

{ #category : #testing }
PRUnspecifiedScriptLanguage >> isSpecified [
	^ false
]

{ #category : #accessing }
PRUnspecifiedScriptLanguage >> originalName [
	^ ''
]
