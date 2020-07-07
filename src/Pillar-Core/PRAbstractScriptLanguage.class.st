"
I represent a programming language that can be referenced from a PRScript to get syntax highlighting.

Check the method  visitScript: for example on PRLatexWriter.  

...
	(self languageForScript: aScript) isSyntaxHighlightingPossible
		ifTrue: [ parameters add: 'language=' , (self languageForScript: aScript) printString ].
...
"
Class {
	#name : #PRAbstractScriptLanguage,
	#superclass : #PRObject,
	#instVars : [
		'originalName'
	],
	#category : #'Pillar-Core-Model'
}

{ #category : #protected }
PRAbstractScriptLanguage class >> acceptLanguage: aLanguageName [
	^ aLanguageName matchesRegexIgnoringCase: self acceptedLanguagesRegex 
]

{ #category : #requirement }
PRAbstractScriptLanguage class >> acceptedLanguagesRegex [ 
	^ self standardName
]

{ #category : #private }
PRAbstractScriptLanguage class >> concreteClassFor: aLanguageName [
	aLanguageName ifNil: [ ^ PRUnspecifiedScriptLanguage ].
	^ (self withAllConcreteClasses copyWithout: PRUnsupportedScriptLanguage)
		detect: [ :cl | cl acceptLanguage: aLanguageName ]
		ifNone: [ PRUnsupportedScriptLanguage ]
]

{ #category : #'instance creation' }
PRAbstractScriptLanguage class >> default [
	^ self new
		originalName: self standardName;
		yourself
]

{ #category : #'instance creation' }
PRAbstractScriptLanguage class >> for: aLanguageName [
	^ (self concreteClassFor: aLanguageName) new
		originalName: aLanguageName;
		yourself
]

{ #category : #protected }
PRAbstractScriptLanguage class >> standardName [
	"Pay attention standardName is used in LaTeX for displaycode as follows
	standardName
	
	[[[language=bash
	...
	]]]
	
	\begin{displaycode}{bash)}
	"
	^ self subclassResponsibility
]

{ #category : #comparing }
PRAbstractScriptLanguage >> = anObject [
	^ self originalName = anObject originalName
]

{ #category : #comparing }
PRAbstractScriptLanguage >> hash [
	^ self originalName hash
]

{ #category : #initialize }
PRAbstractScriptLanguage >> initialize [
	super initialize.
	self originalName: self class standardName
]

{ #category : #testing }
PRAbstractScriptLanguage >> isSpecified [
	^ self subclassResponsibility
]

{ #category : #accessing }
PRAbstractScriptLanguage >> originalName [
	^ originalName
]

{ #category : #accessing }
PRAbstractScriptLanguage >> originalName: aLanguageName [
	originalName := aLanguageName
]

{ #category : #printing }
PRAbstractScriptLanguage >> printOn: aStream [
	"super printOn: aStream.
	aStream nextPutAll: ' ('."
	"do not change this printOn: method because it is used (yes this is not a good idea) for the 
	latex code generation directly...."
	aStream nextPutAll: self originalName.
	"aStream nextPutAll: ')'."
]
