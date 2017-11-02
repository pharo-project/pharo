"
Constants for the FreeType cache
"
Class {
	#name : #FreeTypeCacheConstants,
	#superclass : #SharedPool,
	#classVars : [
		'FreeTypeCacheGlyph',
		'FreeTypeCacheGlyphLCD',
		'FreeTypeCacheGlyphMono',
		'FreeTypeCacheLinearWidth',
		'FreeTypeCacheWidth'
	],
	#category : 'FreeType-Cache'
}

{ #category : #'class initialization' }
FreeTypeCacheConstants class >> initialize [
	"
	FreeTypeCacheConstants initialize
	"
	
	FreeTypeCacheWidth := 0.
	FreeTypeCacheGlyph := 100.	"start at 100 and allow room for 64 subpixel positioned glyphs"
	FreeTypeCacheGlyphLCD := 200. "start at 200 and allow room for 64 subpixel positioned glyphs"
	FreeTypeCacheGlyphMono := 3.
	FreeTypeCacheLinearWidth := 4
	
]
