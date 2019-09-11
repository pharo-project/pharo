"
This is a text renderer, which pipelining a text command(s) , produced by AthensTextComposer
into a concrete calls to canvas.

Its using a glyph renderer instance(s) to draw separate font glyphs on a surface in a most suitable/efficient
manner for given Athens surface.

"
Class {
	#name : #AthensTextRenderer,
	#superclass : #Object,
	#instVars : [
		'text',
		'font',
		'canvas',
		'glyphRenderer',
		'color',
		'advance',
		'maxAscent',
		'currentLine'
	],
	#category : #'Athens-Text'
}

{ #category : #documentation }
AthensTextRenderer class >> textGoryDetails [

"
Took from: http://www.freetype.org/freetype2/docs/tutorial/step2.html

(Font face metrics)
In this case, you can access the global design metrics as:

units_per_EM 	

This is the size of the EM square for the font face. It is used by scalable formats to scale design coordinates to device pixels, as described in the last chapter of this section. Its value usually is 2048 (for TrueType) or 1000 (for Type 1), but others are possible too. It is set to 1 for fixed-size formats like FNT/FON/PCF/BDF.

bbox 	

The global bounding box is defined as the largest rectangle that can enclose all the glyphs in a font face.

ascender 	

The ascender is the vertical distance from the horizontal baseline to the highest ‘character’ coordinate in a font face. Unfortunately, font formats define the ascender differently. For some, it represents the ascent of all capital latin characters (without accents), for others it is the ascent of the highest accented character, and finally, other formats define it as being equal to bbox.yMax.

descender 	

The descender is the vertical distance from the horizontal baseline to the lowest ‘character’ coordinate in a font face. Unfortunately, font formats define the descender differently. For some, it represents the descent of all capital latin characters (without accents), for others it is the ascent of the lowest accented character, and finally, other formats define it as being equal to bbox.yMin. This field is negative for values below the baseline.

height 	

This field is simply used to compute a default line spacing (i.e., the baseline-to-baseline distance) when writing text with this font. Note that it usually is larger than the sum of the ascender and descender taken as absolute values. There is also no guarantee that no glyphs extend above or below subsequent baselines when using this distance.

max_advance_width 	

This field gives the maximum horizontal cursor advance for all glyphs in the font. It can be used to quickly compute the maximum advance width of a string of text. It doesn't correspond to the maximum glyph image width!

max_advance_height 	

Same as max_advance_width but for vertical text layout.

underline_position 	

When displaying or rendering underlined text, this value corresponds to the vertical position, relative to the baseline, of the underline bar's center. It is negative if it is below the baseline.

underline_thickness 	

When displaying or rendering underlined text, this value corresponds to the vertical thickness of the underline.


"
]

{ #category : #initialization }
AthensTextRenderer >> initialize [
	color := Color black.
]

{ #category : #rendering }
AthensTextRenderer >> render: aText lines: lines on: aCanvas [

	text := aText.
	canvas := aCanvas.
	
	canvas pathTransform restoreAfter: [

		lines do: [:line | | origY |
			currentLine := line.
			maxAscent := line maxAscent.
			"set the origin to font's baseline position"
			canvas pathTransform translateX: 0 Y: maxAscent.
			"reset advance"
			advance := 0@0.
			line renderOn: self.
			"line is rendered, advance vertically by line height"
			canvas pathTransform translateX: 0 Y: line height * 1.2 - maxAscent.
		] ]
]

{ #category : #rendering }
AthensTextRenderer >> renderBackground: bkObj [
"self halt."
	canvas setPaint: bkObj color.
	canvas drawShape: ( bkObj start@0 corner: bkObj start + bkObj width @ currentLine heightAndDescent)

]

{ #category : #rendering }
AthensTextRenderer >> renderBackground: aColor width: w [
"self halt."
	canvas setPaint: aColor.
	canvas drawShape: ( advance x@0 corner: advance x + w +1@ currentLine heightAndDescent)

]

{ #category : #rendering }
AthensTextRenderer >> renderCharactersFrom: start to: stop [

	" accumulate advance while rendering spans"
	"canvas drawShape: (advance x @ advance y extent: 2 @ 2)."
	glyphRenderer advance: advance.
	advance := advance + (glyphRenderer renderCharacters: text asString from: start to: stop).
	

]

{ #category : #accessing }
AthensTextRenderer >> setCurrentColor: aColor [
	color = aColor ifTrue: [ ^ self ].
	color := aColor.
"	canvas setPaint: aColor."
	glyphRenderer ifNotNil:  [ glyphRenderer setColor: aColor ].
]

{ #category : #accessing }
AthensTextRenderer >> setCurrentFont: aFont [
	font := aFont.
	glyphRenderer := font glyphRendererOn: canvas surface.
	glyphRenderer 
		advance: advance;
		setColor: color.
]
