"
I'm here for authors to specify they don't want syntax highlighting in their scripts.
"
Class {
	#name : #PRNoScriptLanguage,
	#superclass : #PRAbstractScriptLanguage,
	#category : #'Pillar-Core-Model'
}

{ #category : #requirement }
PRNoScriptLanguage class >> acceptedLanguagesRegex [
	^ '^$|', self standardName
]

{ #category : #testing }
PRNoScriptLanguage class >> isAbstract [
	^ false
]

{ #category : #protected }
PRNoScriptLanguage class >> standardName [
	^ 'no language'
]

{ #category : #testing }
PRNoScriptLanguage >> isSpecified [
	^ true
]
