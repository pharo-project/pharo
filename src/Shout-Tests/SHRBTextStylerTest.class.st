Class {
	#name : 'SHRBTextStylerTest',
	#superclass : 'TestCase',
	#category : 'Shout-Tests-Styling',
	#package : 'Shout-Tests',
	#tag : 'Styling'
}

{ #category : 'tests' }
SHRBTextStylerTest >> testStyleForBackgroundColor [
	"tests a background color can be parsed"
	| table |
	
	table := SHRBTextStyler initialTextAttributesForStyleTable: #(
		(style1 nil nil red)
		(style2 nil nil (red lighter))
		(style3 green bold '0F0000')
	).
	
	self assert: (table at: #style1) first class equals: TextBackgroundColor.
	self assert: (table at: #style1) first color equals: Color red.
	self assert: (table at: #style2) first class equals: TextBackgroundColor.
	self assert: (table at: #style2) first color equals: Color red lighter.

	self 
		assert: ((table at: #style3) detect: [ :each | each class = TextBackgroundColor ]) color 
		equals: (Color fromHexString: '0F0000')
]
