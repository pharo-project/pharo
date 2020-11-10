Class {
	#name : #SpStyleVariableTest,
	#superclass : #TestCase,
	#category : #'Spec2-Adapters-Morphic-Tests'
}

{ #category : #tests }
SpStyleVariableTest >> testParseBasicValue [
	| property |

	property := SpStyleVariableSTONReader fromString: 'Geometry { #height: 25 }'.
	self assert: property height class equals: SpStyleVariable.
	self assert: property height value equals: 25
]

{ #category : #tests }
SpStyleVariableTest >> testParseColorVariable [
	| property |

	property := SpStyleVariableSTONReader fromString: 'Container { #borderColor: #red }'.
	self assert: property borderColor class equals: SpStyleVariable.
	self assert: property borderColor value equals: #red.
	self assert: property borderColor valueAsColor equals: Color red.
	
	property := SpStyleVariableSTONReader fromString: 'Container { 
		#borderColor: Color { #red:1, #green:0, #blue:0, #alpha:1 } }'.
	self assert: property borderColor class equals: SpStyleVariable.
	self assert: property borderColor value equals: Color red.
	self assert: property borderColor valueAsColor equals: Color red

]

{ #category : #tests }
SpStyleVariableTest >> testParseFontVariable [
	| property |

	property := SpStyleVariableSTONReader fromString: 'Font { #name: EnvironmentFont(#default) }'.
	self assert: property name class equals: SpStyleVariableEnvironmentFont.
	self assert: property name value equals: StandardFonts defaultFont.	
	
	property := SpStyleVariableSTONReader fromString: 'Font { #name: EnvironmentFont(#code) }'.
	self assert: property name class equals: SpStyleVariableEnvironmentFont.
	self assert: property name value equals: StandardFonts codeFont.	

]

{ #category : #tests }
SpStyleVariableTest >> testParseResetVariable [
	| property |

	property := SpStyleVariableSTONReader fromString: 'Geometry { #vResizing: Reset }'.
	self assert: property vResizing class equals: SpStyleVariableReset.
	self assert: property vResizing value isNil
]
