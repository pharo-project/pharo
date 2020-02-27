"
A TextFontReference encodes a font change applicable over a given range of text.  
The font reference is absolute:  unlike a TextFontChange, it is independent of the textStyle governing display of this text.

-----exemple--------
| stream |
stream := TextStream on: (Text new: 100).
stream 
	nextPutAll: 'Small is ';
	withAttribute: (TextFontReference toFont: (LogicalFont familyName: 'Source Sans Pro' pointSize: 8) )
	do: [ stream nextPutAll: 'cool' ].
TextMorph new 
	newContents: stream contents;
	openInWindow
"
Class {
	#name : #TextFontReference,
	#superclass : #TextFontChange,
	#instVars : [
		'font'
	],
	#category : #'Text-Core-Attributes'
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
