"
A variable to access environment colors (defined in `UITheme` and subclasses) 
"
Class {
	#name : #SpStyleEnvironmentColorVariable,
	#superclass : #SpStyleEnvironmentVariable,
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #documentation }
SpStyleEnvironmentColorVariable class >> documentExampleCode [

	^ 'Draw { #color: EnvironmentColor(#background) }'
]

{ #category : #documentation }
SpStyleEnvironmentColorVariable class >> documentValidValues [

	^ UITheme selectors
		select: [ :each | 
			(each endsWith: 'Color') 
				and: [ ((UITheme current perform: each) isKindOf: Color) ] ]
		thenCollect: [ :each | each allButLast: 5 ]
]

{ #category : #'ston-core' }
SpStyleEnvironmentColorVariable class >> stonName [

	^ 'EnvironmentColor'
]

{ #category : #evaluating }
SpStyleEnvironmentColorVariable >> value [

	self name ifNil: [ ^ nil ].
	^ Smalltalk ui theme perform: (self name, 'Color') asSymbol
]

{ #category : #evaluating }
SpStyleEnvironmentColorVariable >> valueAsColor [
	
	^ self value
]
