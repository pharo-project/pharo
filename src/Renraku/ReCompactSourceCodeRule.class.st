"
A lint rule to check for unnecessary separators at end of source code lines and unnecessary dot at end of method
"
Class {
	#name : #ReCompactSourceCodeRule,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'testing - interest' }
ReCompactSourceCodeRule class >> checksMethod [

	^ true
]

{ #category : #running }
ReCompactSourceCodeRule >> basicCheck: aMethod [

	^ ReMethodSourceCleaner ableToClean: aMethod
]

{ #category : #testing }
ReCompactSourceCodeRule >> change [

	^' '
]

{ #category : #running }
ReCompactSourceCodeRule >> check: aMethod forCritiquesDo: aCriticBlock [

	(self basicCheck: aMethod)
		ifTrue: [
			aCriticBlock cull: (
				ReCompactSourceCodeCritic
					for: aMethod
					by: self)  ]
]

{ #category : #accessing }
ReCompactSourceCodeRule >> group [

	^ 'Clean Code'
]

{ #category : #accessing }
ReCompactSourceCodeRule >> name [

	^ 'Unnecessary separators or final dots found in method body'
]

{ #category : #testing }
ReCompactSourceCodeRule >> providesChange [

	^ true
]
