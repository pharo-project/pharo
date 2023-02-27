"
do not use this class. this is subject of change or removal
"
Class {
	#name : #AthensCairoText,
	#superclass : #Object,
	#traits : 'TCairoLibrary',
	#classTraits : 'TCairoLibrary classTrait',
	#instVars : [
		'text',
		'fontFamily',
		'fontSlant',
		'fontWeight',
		'fontSize'
	],
	#pools : [
		'AthensCairoDefinitions'
	],
	#category : #'Athens-Cairo-Text'
}

{ #category : #accessing }
AthensCairoText >> asString [
	^ text asString
]

{ #category : #'font description' }
AthensCairoText >> beBold [
	self fontWeight: CAIRO_FONT_WEIGHT_BOLD
]

{ #category : #'font description' }
AthensCairoText >> beNormal [
	self fontWeight: CAIRO_FONT_WEIGHT_NORMAL
]

{ #category : #'font description' }
AthensCairoText >> beSlantNormal [
	self fontSlant: CAIRO_FONT_SLANT_NORMAL
]

{ #category : #private }
AthensCairoText >> convertTextUtf8: aString [
	| encoder buffer |
	buffer := ByteArray new: (aString size + 2) * 4.
	encoder := ZnUTF8Encoder new.
	encoder
		next: aString size
		putAll: aString
		startingAt: 1
		toStream: buffer writeStream.
	^ buffer
]

{ #category : #drawing }
AthensCairoText >> drawOn: aCanvas [
	self loadOn: aCanvas.
	self showOn: aCanvas
]

{ #category : #'font description' }
AthensCairoText >> fontFamily [

	^ fontFamily
]

{ #category : #'font description' }
AthensCairoText >> fontFamily: anObject [
	"CSS2 generic family names: serif, sans-serif, cursive, fantasy, monospace"
	fontFamily := anObject
]

{ #category : #'font description' }
AthensCairoText >> fontSize [

	^ fontSize
]

{ #category : #'font description' }
AthensCairoText >> fontSize: anObject [

	fontSize := anObject
]

{ #category : #'font description' }
AthensCairoText >> fontSlant [

	^ fontSlant
]

{ #category : #'font description' }
AthensCairoText >> fontSlant: anObject [
	anObject isSymbol
		ifTrue:[ fontSlant := self slantFrom: anObject ]
		ifFalse:[ fontSlant := anObject ]
]

{ #category : #'font description' }
AthensCairoText >> fontWeight [

	^ fontWeight
]

{ #category : #'font description' }
AthensCairoText >> fontWeight: anObject [
	anObject isSymbol
		ifTrue:[ fontWeight := self weightFrom: anObject ]
		ifFalse:[ fontWeight :=  anObject ]
]

{ #category : #initialization }
AthensCairoText >> initialize [
	super initialize.

	self beNormal; beSlantNormal.
	 fontSize := 10
]

{ #category : #drawing }
AthensCairoText >> loadOn: aCanvas [

	fontFamily ifNotNil:[
		self
			primSelectFont:  fontFamily
			slant:  fontSlant
			weight:  fontWeight
			on: aCanvas ].

	fontSize ifNotNil: [
		self primSetFontSize: (CairoScaledFont pointsToPixels: fontSize) on: aCanvas ]
]

{ #category : #private }
AthensCairoText >> primSelectFont: aFontFamily
		slant: aFontSlant
		weight: aFontWeight
		on: aCanvas [
	^ self ffiCall: #(void cairo_select_font_face (
			cairo_t aCanvas,
   			char * aFontFamily,
      		cairo_font_slant_t aFontSlant,
        	cairo_font_weight_t aFontWeight) )
]

{ #category : #private }
AthensCairoText >> primSetFontSize: aFontSize on: aCanvas [
	^ self ffiCall: #(void cairo_set_font_size (
			cairo_t aCanvas,
			double aFontSize) )
]

{ #category : #private }
AthensCairoText >> primShowText: aString on: aCanvas [
	^ self ffiCall: #(void cairo_show_text (
			cairo_t aCanvas,
			char * aString) )
]

{ #category : #'font description' }
AthensCairoText >> readFrom: aFontDescription [
	self fontFamily: aFontDescription fontFamily.
	self fontSlant: aFontDescription fontSlant.
	self fontWeight:  aFontDescription fontWeight.
	self fontSize: aFontDescription fontSize
]

{ #category : #drawing }
AthensCairoText >> showOn: aCanvas [
	| utf8String |
	aCanvas setPathMatrix.
	utf8String := self convertTextUtf8: self asString.
	self primShowText: utf8String on: aCanvas
]

{ #category : #private }
AthensCairoText >> slantFrom: aSymbol [
	^ aSymbol caseOf: {
		[#normal]->[ CAIRO_FONT_SLANT_NORMAL ].
 		[#italic]->[   CAIRO_FONT_SLANT_ITALIC ].
		[#oblique]->[ CAIRO_FONT_SLANT_OBLIQUE ] }
]

{ #category : #accessing }
AthensCairoText >> text [

	^ text
]

{ #category : #accessing }
AthensCairoText >> text: anObject [

	text := anObject
]

{ #category : #private }
AthensCairoText >> weightFrom: aSymbol [
	^ aSymbol caseOf: {
		[#normal]->[ CAIRO_FONT_WEIGHT_NORMAL  ].
 		[#bold]->[    CAIRO_FONT_WEIGHT_BOLD ] }
]
