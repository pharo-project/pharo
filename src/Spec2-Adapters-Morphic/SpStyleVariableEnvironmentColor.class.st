"
A variable to access environment colors (defined in `UITheme` and subclasses) 
"
Class {
	#name : #SpStyleVariableEnvironmentColor,
	#superclass : #SpStyleVariableEnvironment,
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #documentation }
SpStyleVariableEnvironmentColor class >> documentExampleCode [

	^ 'Draw { #color: EnvironmentColor(#background) }'
]

{ #category : #documentation }
SpStyleVariableEnvironmentColor class >> documentValidValues [

	^ UITheme selectors
		select: [ :each | 
			(each endsWith: 'Color') 
				and: [ ((UITheme current perform: each) isKindOf: Color) ] ]
		thenCollect: [ :each | each allButLast: 5 ]
]

{ #category : #'ston-core' }
SpStyleVariableEnvironmentColor class >> stonName [

	^ 'EnvironmentColor'
]

{ #category : #evaluating }
SpStyleVariableEnvironmentColor >> value [

	self name ifNil: [ ^ nil ].
	^ Smalltalk ui theme perform: (self name, 'Color') asSymbol
]

{ #category : #evaluating }
SpStyleVariableEnvironmentColor >> valueAsColor [
	
	^ self value
]
