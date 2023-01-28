"
I representing a stroke paint object used by Athens.
My subclasses provide backend-specific implementation.
I am more serving to define the common protocols and
requirements for all backends (such as defaults).


IMPORTANT NOTE: a stroke paint object provides a protocol 
only for setting stroke properties, but not retrieving them back.
This is intentionally, because the way how these properties is
managed are highly backend-specific. 

The stroke paint has following properties:
 - fill paint. A paint to use for filling strokes.
	Can be any athens basic paint, except from stroke one.

 - stroke width. Can be set using #width:

	Default: if width is not set explicitly for stroke paint,
		it is assumed to be equal to 1.0.
		
 - join style. 
		Currently there are 3 kinds of joins supported:
			bevel
			miter
			round
		To set join style for paint, use 
			#joinBevel, #joinMiter or #joinRound methods.

	Default: if join style is not explicitly set for stroke paint,
		it will use bevel join style.

 - cap style
		supported: butt, round and square.
		To set cap style, use
			#capButt, #capRound or #capSquare methods.

	Default: if cap style is not explicitly set for stroke paint,
		it will use butt cap style.

 - miter limit, set with #miterLimit: accessor.
	(default and meaning of limit is not yet determined)

Dashes: 
	stroke can use dashing. 
	Dash is special kind of stroke which won't draw a continuous
	stroke connecting path segments, but instead stroke with
	alternating fill-gap style , defined by provided input.
	
	Protocol:
	
	paint 
		dashes: dashPattern 
		offset: anOffset.
		
	The dash pattern is a simple 
	collection of alternating lengths, like: 
	#(""fill"" 50 ""gap"" 50) 
	
	in this example , first 50 length units of path
	will be filled with stroke, and next 50 will be skipped,
	forming a gap, then again filled and again skipped,
	and so on until path ends.
	The dash pattern can contain as many length elements
	as needed. Just remember that each odd element represents
	length to fill with stroke, and each even element, length to skip,
	while traversing along the path.
	
	The offset controls the shift in length units, 
	relative to path start.
	
	Default: if dash is not explicitly set for stroke paint,
		it will fill the path using continuous stroke (no gaps). 
"
Class {
	#name : #AthensStrokePaint,
	#superclass : #AthensPaint,
	#instVars : [
		'fillPaint',
		'width',
		'miterLimit'
	],
	#category : #'Athens-Core-Paints'
}

{ #category : #converting }
AthensStrokePaint >> asStrokePaintOn: aCanvas [
	"receiver is stroke paint already"
	^ self
]

{ #category : #'setting - cap styles' }
AthensStrokePaint >> capButt [
	"Set this paint to use butt cap style"
	self subclassResponsibility
]

{ #category : #'setting - cap styles' }
AthensStrokePaint >> capRound [
	"Set this paint to use round cap style"
	self subclassResponsibility
]

{ #category : #'setting - cap styles' }
AthensStrokePaint >> capSquare [
	"Set this paint to use square cap style"
	self subclassResponsibility
]

{ #category : #'setting - dashes' }
AthensStrokePaint >> dashes: anAlternateCollectionOfLenghts offset: anOffset [

	'self dashes: #( "fill" 20  "gap" 10 "fill" 35 "gap" 30) offset: 0. '.

	self subclassResponsibility
]

{ #category : #accessing }
AthensStrokePaint >> fillPaint: anObject [

	fillPaint := anObject
]

{ #category : #initialization }
AthensStrokePaint >> initialize [
	super initialize.
	self setDefaults
]

{ #category : #'setting - join styles' }
AthensStrokePaint >> joinBevel [
	"Set receiver to use bevel join style"
	self subclassResponsibility
]

{ #category : #'setting - join styles' }
AthensStrokePaint >> joinMiter [
	"Set receiver to use miter join style"
	self subclassResponsibility
]

{ #category : #'setting - join styles' }
AthensStrokePaint >> joinRound [
	"Set receiver to use round join style"
	self subclassResponsibility
]

{ #category : #accessing }
AthensStrokePaint >> miterLimit: anObject [

	miterLimit := anObject
]

{ #category : #initialization }
AthensStrokePaint >> setDefaults [

	"Set the default values for strokes"
	width := 1.
	self
		capButt;
		joinBevel;
		miterLimit: 4.0
]

{ #category : #accessing }
AthensStrokePaint >> width [
	^ width
]

{ #category : #accessing }
AthensStrokePaint >> width: anObject [
	"set the stroke width"
	width := anObject
]
