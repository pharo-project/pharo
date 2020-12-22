"
A variable to access environment fonts (defined in `StandardFonts class`).
This variable must be used to assign the attribute `name` of a `SpStyleFont` property.
"
Class {
	#name : #SpStyleVariableEnvironmentFont,
	#superclass : #SpStyleVariableEnvironment,
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #documentation }
SpStyleVariableEnvironmentFont class >> documentExampleCode [

	^ 'Font { #name: EnvironmentFont(#default) }'
]

{ #category : #documentation }
SpStyleVariableEnvironmentFont class >> documentValidValues [

	^ StandardFonts class selectors 
		select: [ :each | each endsWith: 'Font' ]
		thenCollect: [ :each | each allButLast: 4 ]
]

{ #category : #'ston-core' }
SpStyleVariableEnvironmentFont class >> stonName [

	^ 'EnvironmentFont'
]

{ #category : #accessing }
SpStyleVariableEnvironmentFont >> familyName [

	^ self value familyName
]

{ #category : #accessing }
SpStyleVariableEnvironmentFont >> name [

	^ super name ifNil: [ #default ]
]

{ #category : #accessing }
SpStyleVariableEnvironmentFont >> pointSize [

	^ self value pointSize
]

{ #category : #evaluating }
SpStyleVariableEnvironmentFont >> value [

	^ StandardFonts perform: (self name, 'Font') asSymbol
]
