"
TextStyleAsFont Family members
"
Class {
	#name : 'TextStyleAsFontFamilyMember',
	#superclass : 'AbstractFontFamilyMember',
	#category : 'Fonts-Infrastructure-Derived',
	#package : 'Fonts-Infrastructure',
	#tag : 'Derived'
}

{ #category : 'comparing' }
TextStyleAsFontFamilyMember >> <= aTextStyleAsFontFamilyMember [

	| orderedItems |
	orderedItems := #( 'Condensed' 'Condensed Italic' 'Condensed Bold'
	                   'Condensed Bold Italic' 'Regular' 'Italic'
	                   'Bold' 'Bold Italic' ).
	^ (orderedItems indexOf: self styleName)
	  <= (orderedItems indexOf: aTextStyleAsFontFamilyMember styleName)
]

{ #category : 'squeak emphasis' }
TextStyleAsFontFamilyMember >> emphasisCode [
	"Answer the emphasis code (1=bold, 2=italic, 3=boldItalic etc"

	| answer |
	answer := 0.
	(styleName includesSubstring: 'Italic') ifTrue: [
		answer := answer bitOr: LogicalFont pharoSlantItalic ].
	(styleName includesSubstring: 'Condensed') ifTrue: [
		answer := answer bitOr: LogicalFont stretchExtraExtended ].
	(styleName includesSubstring: 'Bold') ifTrue: [
		answer := answer bitOr: LogicalFont pharoWeightBold ].
	^ answer
]

{ #category : 'LogicalFont emphasis' }
TextStyleAsFontFamilyMember >> slantValue [

	^ (styleName includesSubstring: 'Italic')
		  ifTrue: [ LogicalFont slantItalic ]
		  ifFalse: [ LogicalFont slantRegular ]
]

{ #category : 'LogicalFont emphasis' }
TextStyleAsFontFamilyMember >> stretchValue [

	^ (styleName includesSubstring: 'Condensed')
		  ifTrue: [ LogicalFont stretchCompressed ]
		  ifFalse: [ LogicalFont stretchRegular ]
]

{ #category : 'LogicalFont emphasis' }
TextStyleAsFontFamilyMember >> weightValue [

	^ (styleName includesSubstring: 'Bold')
		  ifTrue: [ LogicalFont weightBold ]
		  ifFalse: [ LogicalFont weightRegular ]
]
