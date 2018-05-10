"
I'm a transformation of pixel values. I apply up to four masks and shifts to compute the transformed pixel value.

I'm used when applying a BitBlt transfer, whenever pixels from a source to a destination have diferent depths. For further information refer to BitBlt class comments.
"
Class {
	#name : #ColorMap,
	#superclass : #Object,
	#instVars : [
		'shifts',
		'masks',
		'colors'
	],
	#category : #'Colors-Base'
}

{ #category : #'instance creation' }
ColorMap class >> colors: colorArray [
	^self new setShifts: nil masks: nil colors: colorArray
]

{ #category : #'instance creation' }
ColorMap class >> mapBitsFrom: srcBitMask to: dstBitMask [ 
	"Return an array consisting of the shift and the mask for
	mapping component values out of srcBitMask and into dstBitMask.
	While this computation is somewhat complicated it eases the batch
	conversion of all the pixels in BitBlt."
	| srcBits dstBits srcLow srcHigh dstLow dstHigh bits mask shift |
	(srcBitMask = 0 or: [ dstBitMask = 0 ]) ifTrue: 
		[ ^ #(0 0 ) ].	"Zero mask and shift"
	"Compute low and high bit position for source and dest bit mask"
	srcLow := srcBitMask lowBit - 1.
	srcHigh := srcBitMask highBit.
	dstLow := dstBitMask lowBit - 1.
	dstHigh := dstBitMask highBit.
	"Compute the number of bits in source and dest bit mask"
	srcBits := srcHigh - srcLow.
	dstBits := dstHigh - dstLow.
	"Compute the maximum number of bits we can transfer inbetween"
	bits := srcBits min: dstBits.
	"Compute the (unshifted) transfer mask"
	mask := (1 bitShift: bits) - 1.
	"Shift the transfer mask to the mask the highest n bits of srcBitMask"
	mask := mask bitShift: srcHigh - bits.
	"Compute the delta shift so that the most significant bit of the
	source bit mask falls on the most significant bit of the dest bit mask.
	Note that delta is used for #bitShift: so
		shift > 0 : shift right
		shift < 0 : shift left
	e.g., if dstHigh > srcHigh we need to shift left and if dstHigh < srcHigh
	we need to shift right. This leads to:"
	shift := dstHigh - srcHigh.
	"And that's all we need"
	^ Array 
		with: shift
		with: mask
]

{ #category : #'instance creation' }
ColorMap class >> mappingFrom: srcBitMasks to: dstBitMasks [ 
	"Return a color map mapping from the array of source bit masks
	to the array of dest bit masks."
	| shifts masks shiftAndMask |
	shifts := IntegerArray new: 4.
	masks := WordArray new: 4.
	1 
		to: 4
		do: 
			[ :i | 
			shiftAndMask := self 
				mapBitsFrom: (srcBitMasks at: i)
				to: (dstBitMasks at: i).
			shifts 
				at: i
				put: (shiftAndMask at: 1).
			masks 
				at: i
				put: (shiftAndMask at: 2) ].
	^ self 
		shifts: shifts
		masks: masks
]

{ #category : #'instance creation' }
ColorMap class >> mappingFromARGB: dstBitMasks [
	"Return a ColorMap mapping from canonical ARGB space into dstBitMasks"
	^self mappingFrom: #(16rFF0000 16rFF00 16rFF 16rFF000000) to: dstBitMasks
]

{ #category : #'instance creation' }
ColorMap class >> mappingToARGB: srcBitMasks [
	"Return a ColorMap mapping from srcBitMasks into canonical ARGB space"
	^self mappingFrom: srcBitMasks to: #(16rFF0000 16rFF00 16rFF 16rFF000000)
]

{ #category : #'instance creation' }
ColorMap class >> masks: maskArray shifts: shiftArray [
	^self shifts: shiftArray masks: maskArray colors: nil.
]

{ #category : #'instance creation' }
ColorMap class >> shifts: shiftArray masks: maskArray [
	^self shifts: shiftArray masks: maskArray colors: nil.
]

{ #category : #'instance creation' }
ColorMap class >> shifts: shiftArray masks: maskArray colors: colorArray [
	^self new setShifts: shiftArray masks: maskArray colors: colorArray
]

{ #category : #comparing }
ColorMap >> = aColorMap [
	"Return true if the receiver is equal to aColorMap"
	self species == aColorMap species ifFalse:[^false].
	self isIndexed == aColorMap isIndexed ifFalse:[^false].
	^self colors = aColorMap colors and:[
		self shifts = aColorMap shifts and:[
			self masks = aColorMap masks]]
]

{ #category : #accessing }
ColorMap >> alphaMask [
	^masks at: 4
]

{ #category : #accessing }
ColorMap >> alphaMask: value [
	masks at: 4 put: value
]

{ #category : #accessing }
ColorMap >> alphaShift [
	^shifts at: 4
]

{ #category : #accessing }
ColorMap >> alphaShift: value [
	shifts at: 4 put: value
]

{ #category : #accessing }
ColorMap >> at: index [
	^colors at: index
]

{ #category : #accessing }
ColorMap >> at: index put: value [
	^colors at: index put: value
]

{ #category : #accessing }
ColorMap >> blueMask [
	^masks at: 3
]

{ #category : #accessing }
ColorMap >> blueMask: value [
	masks at: 3 put: value
]

{ #category : #accessing }
ColorMap >> blueShift [
	^shifts at: 3
]

{ #category : #accessing }
ColorMap >> blueShift: value [
	shifts at: 3 put: value
]

{ #category : #accessing }
ColorMap >> colors [
	^colors
]

{ #category : #accessing }
ColorMap >> greenMask [
	^masks at: 2
]

{ #category : #accessing }
ColorMap >> greenMask: value [
	masks at: 2 put: value
]

{ #category : #accessing }
ColorMap >> greenShift [
	^shifts at: 2
]

{ #category : #accessing }
ColorMap >> greenShift: value [
	shifts at: 2 put: value.
]

{ #category : #comparing }
ColorMap >> hash [
	"Hash is re-implemented because #= is re-implemented"
	^colors hash bitXor: (shifts hash bitXor: masks hash)
]

{ #category : #accessing }
ColorMap >> inverseMap [
	"Return the inverse map of the receiver"
	| newMasks newShifts |
	colors ifNotNil: [ ^ self error: 'Not yet implemented' ].
	newMasks := (Array new: 4) writeStream.
	newShifts := (Array new: 4) writeStream.
	masks 
		with: shifts
		do: 
			[ :mask :shift | 
			newMasks nextPut: (mask bitShift: shift).
			newShifts nextPut: shift negated ].
	^ ColorMap 
		shifts: newShifts contents
		masks: newMasks contents
]

{ #category : #testing }
ColorMap >> isColormap [
	^true
]

{ #category : #testing }
ColorMap >> isFixed [
	"Return true if the receiver does not use a lookup mechanism for pixel mapping"
	^self isIndexed not
]

{ #category : #testing }
ColorMap >> isIndexed [
	"Return true if the receiver uses a lookup mechanism for pixel mapping"
	^colors notNil
]

{ #category : #'pixel mapping' }
ColorMap >> mapPixel: pixelValue [ 
	"Perform a forward pixel mapping operation"
	| pv |
	(shifts == nil and: [ masks == nil ]) 
		ifFalse: 
			[ pv := (((pixelValue bitAnd: self redMask) bitShift: self redShift) bitOr: ((pixelValue bitAnd: self greenMask) bitShift: self greenShift)) bitOr: (((pixelValue bitAnd: self blueMask) bitShift: self blueShift) bitOr: ((pixelValue bitAnd: self alphaMask) bitShift: self alphaShift)) ]
		ifTrue: [ pv := pixelValue ].
	colors ifNotNil: [ pv := colors at: pv ].
	"Need to check for translucency else Form>>paint goes gaga"
	pv = 0 ifTrue: [ pixelValue = 0 ifFalse: [ pv := 1 ] ].
	^ pv
]

{ #category : #'pixel mapping' }
ColorMap >> mappingTo: aColorMap [ 
	"Compute a new color map through the receiver and aColorMap.
	Both maps are assumed to be mappings into canonical ARGB space"
	| fixedMap |
	self = aColorMap ifTrue: [ ^ nil ].	"No mapping needed"
	aColorMap isIndexed ifTrue: [ ^ nil ].	"We can't compute mappings to an indexed map yet"
	fixedMap := self class 
		mappingFrom: self rgbaBitMasks
		to: aColorMap rgbaBitMasks.
	self isIndexed ifFalse: [ ^ fixedMap ].
	"If the receiver is indexed then we need to map the colors as well"
	self flag: #untested.
	^ ColorMap 
		shifts: fixedMap shifts
		masks: fixedMap masks
		colors: (colors collect: [ :pv | aColorMap pixelMap: pv ])
]

{ #category : #accessing }
ColorMap >> masks [
	^masks
]

{ #category : #'pixel mapping' }
ColorMap >> pixelMap: pixelValue [ 
	"Perform a reverse pixel mapping operation"
	| pv |
	colors == nil 
		ifTrue: [ pv := pixelValue ]
		ifFalse: [ pv := colors at: pixelValue ].
	(shifts == nil and: [ masks == nil ]) ifFalse: 
		[ pv := (((pv bitAnd: self redMask) bitShift: self redShift) bitOr: ((pv bitAnd: self greenMask) bitShift: self greenShift)) bitOr: (((pv bitAnd: self blueMask) bitShift: self blueShift) bitOr: ((pv bitAnd: self alphaMask) bitShift: self alphaShift)) ].
	"Need to check for translucency else Form>>paint goes gaga"
	pv = 0 ifTrue: [ pixelValue = 0 ifFalse: [ pv := 1 ] ].
	^ pv
]

{ #category : #accessing }
ColorMap >> redMask [
	^masks at: 1
]

{ #category : #accessing }
ColorMap >> redMask: value [
	masks at: 1 put: value
]

{ #category : #accessing }
ColorMap >> redShift [
	^shifts at: 1
]

{ #category : #accessing }
ColorMap >> redShift: value [
	shifts at: 1 put: value
]

{ #category : #accessing }
ColorMap >> rgbaBitMasks [
	"Return the rgba bit masks for the receiver"
	^masks asArray with: shifts collect:[:m :s| m bitShift: s]
]

{ #category : #private }
ColorMap >> setShifts: shiftArray masks: maskArray colors: colorArray [ 
	shiftArray ifNotNil: [ shifts := shiftArray asIntegerArray ].
	maskArray ifNotNil: [ masks := maskArray asWordArray ].
	colorArray ifNotNil: [ colors := colorArray asWordArray ]
]

{ #category : #accessing }
ColorMap >> shifts [
	^shifts
]
