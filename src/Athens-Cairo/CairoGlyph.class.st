"
I correspond to `cairo_glyph_t` structure:

```
typedef struct {
    unsigned long        index;
    double               x;
    double               y;
} cairo_glyph_t;
```
"
Class {
	#name : #CairoGlyph,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_INDEX',
		'OFFSET_X',
		'OFFSET_Y'
	],
	#category : #'Athens-Cairo-Text'
}

{ #category : #'fields description' }
CairoGlyph class >> fieldsDesc [
	"self rebuildFieldAccessors"
	^ #(
	ulong        index;
	double       x;
	double       y;
	)
]

{ #category : #'accessing - structure variables' }
CairoGlyph >> index [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_INDEX
]

{ #category : #'accessing - structure variables' }
CairoGlyph >> index: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_INDEX put: anObject
]

{ #category : #'accessing - structure variables' }
CairoGlyph >> x [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_X
]

{ #category : #'accessing - structure variables' }
CairoGlyph >> x: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_X put: anObject
]

{ #category : #'accessing - structure variables' }
CairoGlyph >> y [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_Y
]

{ #category : #'accessing - structure variables' }
CairoGlyph >> y: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_Y put: anObject
]
