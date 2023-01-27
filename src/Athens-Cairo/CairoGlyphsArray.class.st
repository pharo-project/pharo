"
Captain obvious tells this is an array of CairoGlyph(s),
corresponds to cairo_glyph_t structure:

```
typedef struct {
    unsigned long        index;
    double               x;
    double               y;
} cairo_glyph_t;
```
"
Class {
	#name : #CairoGlyphsArray,
	#superclass : #FFIExternalArray,
	#instVars : [
		'allocatedByCairo'
	],
	#category : #'Athens-Cairo-Text'
}

{ #category : #'instance creation' }
CairoGlyphsArray class >> externalNew: size [
	^ self externalNewType: CairoGlyph size: size
]

{ #category : #'instance creation' }
CairoGlyphsArray class >> new [
	^ self new: nil
]

{ #category : #'instance creation' }
CairoGlyphsArray class >> new: size [
	^ self newType: CairoGlyph size: size
]

{ #category : #accessing }
CairoGlyphsArray >> allocatedByCairo [
	^ allocatedByCairo
]

{ #category : #accessing }
CairoGlyphsArray >> allocatedByCairo: aBoolean [
	allocatedByCairo := aBoolean
]
