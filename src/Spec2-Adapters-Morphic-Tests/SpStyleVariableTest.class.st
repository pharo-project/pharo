Class {
	#name : #SpStyleVariableTest,
	#superclass : #TestCase,
	#category : #'Spec2-Adapters-Morphic-Tests'
}

{ #category : #tests }
SpStyleVariableTest >> testParseBasicValue [
	| property |

	property := SpStyleVariableSTONReader fromString: 'Geometry { #height: 25 }'.
	self assert: property height equals: 25.
	self assert: property heightVariable value equals: 25
]

{ #category : #tests }
SpStyleVariableTest >> testParseColorVariable [
	| property |

	property := SpStyleVariableSTONReader fromString: 'Container { #borderColor: #red }'.
	self assert: property borderColor equals: #red.
	self assert: property borderColorVariable value equals: #red.
	self assert: property borderColorVariable valueAsColor equals: Color red.
	
	property := SpStyleVariableSTONReader fromString: 'Container { 
		#borderColor: Color { #red:1, #green:0, #blue:0, #alpha:1 } }'.
	self assert: property borderColor equals: Color red.
	self assert: property borderColorVariable value equals: Color red.
	self assert: property borderColorVariable valueAsColor equals: Color red
]

{ #category : #tests }
SpStyleVariableTest >> testParseFontVariable [
	| property |
	
	property := SpStyleVariableSTONReader fromString: 'Font { #name: EnvironmentFont(#code) }'.
	self assert: property nameVariable class equals: SpStyleVariableEnvironmentFont.
	self assert: property nameVariable value equals: StandardFonts codeFont.	

]

{ #category : #tests }
SpStyleVariableTest >> testParseResetVariable [
	| property |

	property := SpStyleVariableSTONReader fromString: 'Geometry { #vResizing: Reset }'.
	self assert: property vResizing isNil.
	self assert: property vResizingVariable value isNil
]
