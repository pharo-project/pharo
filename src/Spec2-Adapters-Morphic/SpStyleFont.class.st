"
Font property (`SpStyleFont`)

Font properties control how a component (morph) with font will draw the text.
I keep this properties: 

- name: The font name (it needs to be available in the list of fonts, e.g. ""Source Code Pro"") 
- size: The font point size.
- bold: Font is bold? (boolean, default """"false"""")
- italic: Font is italic? (boolean, default """"false"""")

## Usage
The identifier of font in the stylesheet is _Font_.

```language=Javascript
Font { 
 	#name: ""Source Sans Pro"",  
	#size: 12,
	#bold: false,
	#italic: false
}
```

## Predefined fonts
User can also point to the fonts pre-defined (comming from Settings).
To do that, is enough to access the property `predefinedFont`, and value can be one of the list: 

- `#default`
- `#code`
- `#ballon`
- `#button`
- `#halo`
- `#list`
- `#menu`

**NOTE:** Using `predefinedFont` will meke any other Font property defined to be ignored.

```language=Javascript
Font { 
 	#predefinedFont: #code
}
```

"
Class {
	#name : #SpStyleFont,
	#superclass : #SpStyleProperty,
	#instVars : [
		'predefinedFont',
		'name',
		'size',
		'italic',
		'bold',
		'color'
	],
	#classVars : [
		'FontCache'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #private }
SpStyleFont class >> addFontToCache: aFont [

	self fontCache add: aFont
]

{ #category : #private }
SpStyleFont class >> fontCache [

	^ FontCache ifNil: [ FontCache := WeakSet new ]
]

{ #category : #'ston-core' }
SpStyleFont class >> stonName [
	
	^ 'Font'
]

{ #category : #private }
SpStyleFont >> addFontToCache: aFont [

	self class addFontToCache: aFont
]

{ #category : #private }
SpStyleFont >> anyFontDecorator [
	
	^ color notNil 
		or: [ size notNil 
		or: [ bold notNil 
		or: [ italic notNil ] ] ]
]

{ #category : #operations }
SpStyleFont >> applyTo: aMorph [

	"not all morphs understand font, I need to verify it (which is not cool verifying a 
	 selector but other way would be to define #font: in Morph and I do not want that)"
	(aMorph respondsTo: #font:) ifFalse: [ ^ self ].
	
	aMorph font: self definedFont.
	
	self color ifNil: [ ^ self ].
	"Again, not all morphs understand #textColor: and I need to verify it is there."
	(aMorph respondsTo: #textColor:) ifFalse: [ ^ self ].
	aMorph textColor: self colorVariable valueAsColor
]

{ #category : #accessing }
SpStyleFont >> bold [

	^ self boldVariable value
]

{ #category : #accessing }
SpStyleFont >> bold: aBoolean [

	bold := aBoolean asStyleVariable
]

{ #category : #'accessing variables' }
SpStyleFont >> boldVariable [

	^ self toVariable: bold
]

{ #category : #private }
SpStyleFont >> calculateDefinedFont [
	| font |
	
	font := LogicalFont
		familyName: self calculateFontName
		pointSize: self calculateFontSize.

	self isItalic ifTrue: [ font forceItalicOrOblique ].
	self isBold ifTrue: [ font forceBold ].

	^ font
]

{ #category : #private }
SpStyleFont >> calculateFontName [
	| var |
	
	var := self nameVariable.
	^ var isEnvironmentVariable 
		ifTrue: [ var familyName ] 
		ifFalse: [ var value ]
]

{ #category : #private }
SpStyleFont >> calculateFontSize [
	
	self sizeVariable ifNotNil: [ :aVariable |
		aVariable value ifNotNil: [ 
			^ self sizeVariable value ] ].
	self nameVariable isEnvironmentVariable ifTrue: [ ^ self nameVariable pointSize ].
	self hasPredefinedFont ifTrue: [ ^ self obtainPredefinedFont pointSize ].

	^ nil
]

{ #category : #accessing }
SpStyleFont >> color [
  "This is meant to set the text color.
	This property can be expressed as 
	- a STON map: ==Color { #red : 1., #green : 0, #blue : 0, #alpha : 1 }==
	- a named selector: ==#red==
	- an hex string: =='FF0000'=="

	^ self colorVariable value
]

{ #category : #accessing }
SpStyleFont >> color: aColorOrSymbol [

	color := aColorOrSymbol asStyleVariable
]

{ #category : #'accessing variables' }
SpStyleFont >> colorVariable [
  "This is meant to set the text color.
	This property can be expressed as 
	- a STON map: ==Color { #red : 1., #green : 0, #blue : 0, #alpha : 1 }==
	- a named selector: ==#red==
	- an hex string: =='FF0000'=="

	^ self toVariable: color
]

{ #category : #private }
SpStyleFont >> definedFont [
	| definedFont |

	(self predefinedFont notNil and: [ self anyFontDecorator not ]) ifTrue: [ 
		^ self obtainPredefinedFont  ].

	self withCachedFontDo: [ :aFont | ^ aFont ].
	
	definedFont := self calculateDefinedFont.
	self addFontToCache: definedFont.
	^ definedFont
]

{ #category : #private }
SpStyleFont >> hasPredefinedFont [

	^ self predefinedFont notNil
]

{ #category : #testing }
SpStyleFont >> isBold [
	"property may be nil, we verify with strict comparisson"
	
	^ self boldVariable value == true
]

{ #category : #testing }
SpStyleFont >> isItalic [
	"property may be nil, we verify with strict comparisson"
	
	^ self italicVariable value == true 
]

{ #category : #accessing }
SpStyleFont >> italic [

	^ self italicVariable value
]

{ #category : #accessing }
SpStyleFont >> italic: aBoolean [

	italic := aBoolean asStyleVariable
]

{ #category : #'accessing variables' }
SpStyleFont >> italicVariable [

	^ italic
]

{ #category : #operations }
SpStyleFont >> mergeWith: otherProperty [
	| merged |

	merged := super mergeWith: otherProperty.

	"Ensure predefined font will be overriden even if nil."
	otherProperty nameVariable 
		ifNotNil: [ :aName |
			merged 
				writeSlotNamed: 'predefinedFont' value: nil;
				writeSlotNamed: 'name' value: aName ].
		
	^ merged
]

{ #category : #accessing }
SpStyleFont >> name [

	^ self nameVariable value
]

{ #category : #accessing }
SpStyleFont >> name: aString [

	name := aString asStyleVariable
]

{ #category : #'accessing variables' }
SpStyleFont >> nameVariable [

	^ name
]

{ #category : #private }
SpStyleFont >> obtainPredefinedFont [

	^ StandardFonts perform: (self predefinedFont value, 'Font') asSymbol
]

{ #category : #accessing }
SpStyleFont >> predefinedFont [

	^ predefinedFont
]

{ #category : #accessing }
SpStyleFont >> predefinedFont: aSymbol [

	predefinedFont := aSymbol
]

{ #category : #accessing }
SpStyleFont >> size [

	^ self sizeVariable value
]

{ #category : #accessing }
SpStyleFont >> size: aNumber [

	size := aNumber asStyleVariable
]

{ #category : #'accessing variables' }
SpStyleFont >> sizeVariable [

	^ size
]

{ #category : #private }
SpStyleFont >> withCachedFontDo: aBlock [

	self class fontCache 
		detect: [ :each | 
			each familyName = self name 
				and: [ each pointSize = self size 
				and: [ each isBold = self isBold
				and: [ each isItalic = self isItalic ] ] ] ]
		ifFound: aBlock
]
