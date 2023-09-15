"
A BitBltDisplayScanner displays characters on Screen or other Form with help of a BitBlt.

Instance Variables
	bitBlt:		<BitBlt>
	fillBlt:		<BitBlt>

bitBlt
	- the object which knows how to copy bits from one Form (the font glyph data) to another (the destination Form)

fillBlt
	- another object for copying form bits, initialized for displaying the background.


"
Class {
	#name : #BitBltDisplayScanner,
	#superclass : #DisplayScanner,
	#instVars : [
		'bitBlt',
		'scale'
	],
	#category : #'Text-Scanning-Base'
}

{ #category : #displaying }
BitBltDisplayScanner >> displayEmbeddedForm: aForm [

	| scaledForm |

	scaledForm := self scale = 1 ifTrue: [ aForm ] ifFalse: [ aForm scaledToSize: aForm extent * self scale ].
	scaledForm
		displayOn: bitBlt destForm
		at: destX @ (lineY + (line baseline * self scale) - scaledForm height)
		clippingBox: bitBlt clipRect
		rule: Form blend
		fillColor: Color white
]

{ #category : #displaying }
BitBltDisplayScanner >> displayString: string from: startIndex to: stopIndex at: aPoint [
    | endPoint top |
   top := aPoint translateBy: 0@ (font ascent * self scale).
	endPoint := font displayString: string on: bitBlt
		from: startIndex
		to: stopIndex
		at: aPoint kern: kern
		baselineY: aPoint y + (font ascent * self scale)
		scale: self scale.

    (emphasisCode allMask: 4)
        ifTrue: [ font displayUnderlineOn: bitBlt from: top to: endPoint scale: self scale ].
    (emphasisCode allMask: 16)
        ifTrue: [ font displayStrikeoutOn: bitBlt from: top to: endPoint scale: self scale ]
]

{ #category : #initialization }
BitBltDisplayScanner >> initialize [

	super initialize.
	scale := 1
]

{ #category : #accessing }
BitBltDisplayScanner >> scale [

	^ scale
]

{ #category : #accessing }
BitBltDisplayScanner >> scale: newScale [

	scale := newScale
]

{ #category : #private }
BitBltDisplayScanner >> setFont [
	super setFont.  "Sets font and emphasis bits, and maybe foregroundColor"
	font installOn: bitBlt foregroundColor: foregroundColor backgroundColor: Color transparent scale: self scale
]

{ #category : #private }
BitBltDisplayScanner >> setPort: aBitBlt [
	"Install the BitBlt to use"
	bitBlt := aBitBlt.
	bitBlt sourceX: 0; width: 0.	"Init BitBlt so that the first call to a primitive will not fail"
	bitBlt sourceForm: nil. "Make sure font installation won't be confused"
]
