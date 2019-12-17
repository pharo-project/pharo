"
Morph and Model subclasses methods should access GUI theme via 'self theme' and not 'Smalltalk ui theme'.
"
Class {
	#name : #ReThemeAccess,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'testing-interest' }
ReThemeAccess class >> checksMethod [
	^ true
]

{ #category : #'class initialization' }
ReThemeAccess class >> initialize [
   " ReRuleManager cleanUp "
]

{ #category : #running }
ReThemeAccess >> check: aMethod forCritiquesDo: aCriticBlock [ 
	| problemThemes |
	((aMethod methodClass inheritsFrom: Morph) 
		or: [aMethod methodClass inheritsFrom: Model]) ifFalse: [ ^ self ].
		
	problemThemes := 
		(aMethod sendNodes select: [ :msgSend | msgSend selector = #theme 
				and: [msgSend receiver sourceCode withSeparatorsCompacted = 'Smalltalk ui'] ]).
	
	problemThemes do: [ :msgSend |
			aCriticBlock cull: (self 
				createTrivialCritiqueOn: aMethod 
				intervalOf: msgSend 
				hint: msgSend selector asString) ]
]

{ #category : #running }
ReThemeAccess >> group [
	^ 'API Hints'
]

{ #category : #running }
ReThemeAccess >> name [
	^ 'Access to UI theme.'
]

{ #category : #running }
ReThemeAccess >> severity [
	^ #warning
]
