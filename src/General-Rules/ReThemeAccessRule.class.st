"
Morph and Model subclasses methods should access GUI theme via 'self theme' instead of 'Smalltalk ui theme'.
"
Class {
	#name : 'ReThemeAccessRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-API Hints',
	#package : 'General-Rules',
	#tag : 'API Hints'
}

{ #category : 'testing - interest' }
ReThemeAccessRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReThemeAccessRule class >> group [
	^ 'API Hints'
]

{ #category : 'class initialization' }
ReThemeAccessRule class >> initialize [
   " ReRuleManager cleanUp "
]

{ #category : 'accessing' }
ReThemeAccessRule class >> ruleName [
	^ 'Access to UI theme.'
]

{ #category : 'running' }
ReThemeAccessRule >> check: aMethod forCritiquesDo: aCriticBlock [
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
