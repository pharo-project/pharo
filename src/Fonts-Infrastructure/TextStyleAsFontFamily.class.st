"
Text style as font family
"
Class {
	#name : #TextStyleAsFontFamily,
	#superclass : #AbstractFontFamily,
	#instVars : [
		#textStyle
	],
	#category : #'Fonts-Infrastructure-Derived'
}

{ #category : #accessing }
TextStyleAsFontFamily >> members [

	^ members ifNil: [
		  members := #( 'Regular' 'Italic' 'Bold' 'Bold Italic' ) collect: [
			             :each |
			             TextStyleAsFontFamilyMember new
				             family: self;
				             styleName: each;
				             yourself ] ]
]

{ #category : #accessing }
TextStyleAsFontFamily >> textStyle [
	"Answer the value of textStyle"

	^ textStyle
]

{ #category : #accessing }
TextStyleAsFontFamily >> textStyle: anObject [
	"Set the value of textStyle"

	textStyle := anObject
]
