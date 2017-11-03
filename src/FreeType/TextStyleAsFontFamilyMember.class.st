"
TextStyleAsFont Family members
"
Class {
	#name : #TextStyleAsFontFamilyMember,
	#superclass : #FontFamilyMemberAbstract,
	#category : #FreeType-FontManager
}

{ #category : #comparing }
TextStyleAsFontFamilyMember >> <=  aTextStyleAsFontFamilyMember [
	| orderedItems |
	orderedItems := #('Condensed' 'Condensed Italic' 'Condensed Bold'  'Condensed Bold Italic' 'Regular' 'Italic' 'Bold' 'Bold Italic').
	^(orderedItems indexOf: self styleName) <= (orderedItems indexOf: aTextStyleAsFontFamilyMember styleName)
]

{ #category : #'squeak emphasis' }
TextStyleAsFontFamilyMember >> emphasisCode [
	" Answer the squeak emphasis code (1=bold, 2=italic, 3=boldItalic etc"
	| answer |
	answer := 0.
	(styleName includesSubstring: 'Italic') 
		ifTrue:[answer := answer bitOr: LogicalFont squeakSlantItalic].
	(styleName includesSubstring: 'Condensed')
		ifTrue:[answer := answer bitOr: LogicalFont squeakStretchCondensed].
	(styleName includesSubstring: 'Bold')
		ifTrue:[answer := answer bitOr: LogicalFont squeakWeightBold].
	^answer
]

{ #category : #testing }
TextStyleAsFontFamilyMember >> simulated [ 
	^false
]

{ #category : #'LogicalFont emphasis' }
TextStyleAsFontFamilyMember >> slantValue [
	^(styleName includesSubstring: 'Italic') 
		ifTrue:[LogicalFont slantItalic] 
		ifFalse:[LogicalFont slantRegular]
]

{ #category : #'LogicalFont emphasis' }
TextStyleAsFontFamilyMember >> stretchValue [
	^(styleName includesSubstring: 'Condensed') 
		ifTrue:[LogicalFont stretchCompressed]
		ifFalse:[LogicalFont stretchRegular]
]

{ #category : #'LogicalFont emphasis' }
TextStyleAsFontFamilyMember >> weightValue [
	^(styleName includesSubstring: 'Bold')
		ifTrue:[LogicalFont weightBold] 
		ifFalse:[LogicalFont weightRegular]
]
