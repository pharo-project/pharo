"
A TextFontReference encodes a font change applicable over a given range of text.  The font reference is absolute:  unlike a TextFontChange, it is independent of the textStyle governing display of this text.
"
Class {
	#name : #TextFontReference,
	#superclass : #TextFontChange,
	#instVars : [
		'font'
	],
	#category : 'Text-Core'
}

{ #category : #'instance creation' }
TextFontReference class >> toFont: aFont [
	^ self new toFont: aFont
]

{ #category : #comparing }
TextFontReference >> = other [ 
	^ (other class == self class) 
		and: [other font = font]
]

{ #category : #accessing }
TextFontReference >> font [

	^ font
]

{ #category : #styling }
TextFontReference >> forFontInStyle: aTextStyle do: aBlock [
	aBlock value: font
]

{ #category : #comparing }
TextFontReference >> hash [
	"#hash is re-implemented because #= is re-implemented"
	^font hash
]

{ #category : #printing }
TextFontReference >> printOn: aStream [
	aStream nextPutAll: 'a TextFontReference(';
		print: font;
		nextPut: $)
]

{ #category : #accessing }
TextFontReference >> toFont: aFont [

	font := aFont
]

{ #category : #utilities }
TextFontReference >> writeScanOn: strm [

	strm nextPut: $F.
	strm nextPutAll: font familyName; nextPut: $#.
	font height printOn: strm.
]
