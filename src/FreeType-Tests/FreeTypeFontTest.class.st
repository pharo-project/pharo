Class {
	#name : 'FreeTypeFontTest',
	#superclass : 'TestCase',
	#category : 'FreeType-Tests-Fonts',
	#package : 'FreeType-Tests',
	#tag : 'Fonts'
}

{ #category : 'tests' }
FreeTypeFontTest >> testReleaseCachedState [

	| font glyphBlock glyphFormXScale1 glyphFormYScale1 glyphFormXScale2 |

	font := FreeTypeFont fromBytes: SourceSansProRegular fontContents pointSize: 30.
	glyphBlock := [ :character :scale |
		font glyphOf: character colorValue: (Color black pixelValueForDepth: 32)
			mono: false subpixelPosition: 0 scale: scale ].
	glyphFormXScale1 := glyphBlock value: $X value: 1.
	glyphFormYScale1 := glyphBlock value: $Y value: 1.
	glyphFormXScale2 := glyphBlock value: $X value: 2.
	self assert: (glyphBlock value: $X value: 1) identicalTo: glyphFormXScale1.
	self assert: (glyphBlock value: $Y value: 1) identicalTo: glyphFormYScale1.
	self assert: (glyphBlock value: $X value: 2) identicalTo: glyphFormXScale2.
	font releaseCachedState.
	self deny: (glyphBlock value: $X value: 1) identicalTo: glyphFormXScale1.
	self deny: (glyphBlock value: $Y value: 1) identicalTo: glyphFormYScale1.
	self deny: (glyphBlock value: $X value: 2) identicalTo: glyphFormXScale2.
]
