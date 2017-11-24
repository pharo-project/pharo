"
I represent a single component of color in JPEG YCbCr color space.  I can accept a list of blocks in my component from the current MCU, then stream the samples from this block for use in color conversion.  I also store the running DC sample value for my component, used by the Huffman decoder.

The following layout is fixed for the JPEG primitives to work:
	currentX 		<SmallInteger>
	currentY 		<SmallInteger>
	hSampleFactor 	<SmallInteger>
	vSampleFactor 	<SmallInteger>
	mcuBlocks 		<Array of: <IntegerArray of: DCTSize2 * Integer>>
	widthInBlocks 	<SmallInteger>
	heightInBlocks 	<SmallInteger>
	dctSize 			<SmallInteger>
	mcuWidth 		<SmallInteger>
	mcuHeight 		<SmallInteger>
	priorDCValue 	<SmallInteger>

"
Class {
	#name : #JPEGColorComponent,
	#superclass : #Object,
	#instVars : [
		'currentX',
		'currentY',
		'hSampleFactor',
		'vSampleFactor',
		'mcuBlocks',
		'widthInBlocks',
		'heightInBlocks',
		'dctSize',
		'mcuWidth',
		'mcuHeight',
		'priorDCValue',
		'id',
		'qTableIndex',
		'dcTableIndex',
		'acTableIndex'
	],
	#category : #'Graphics-Files'
}

{ #category : #accessing }
JPEGColorComponent >> acTableIndex [

	^acTableIndex
]

{ #category : #accessing }
JPEGColorComponent >> acTableIndex: anInteger [ 
	acTableIndex := anInteger
]

{ #category : #accessing }
JPEGColorComponent >> dcTableIndex [

	^dcTableIndex
]

{ #category : #accessing }
JPEGColorComponent >> dcTableIndex: anInteger [ 
	dcTableIndex := anInteger
]

{ #category : #accessing }
JPEGColorComponent >> heightInBlocks [

	^heightInBlocks
]

{ #category : #accessing }
JPEGColorComponent >> heightInBlocks: anInteger [ 
	heightInBlocks := anInteger
]

{ #category : #accessing }
JPEGColorComponent >> id [

	^id
]

{ #category : #accessing }
JPEGColorComponent >> id: anObject [ 
	id := anObject
]

{ #category : #'sample streaming' }
JPEGColorComponent >> initializeSampleStreamBlocks: aCollection [ 
	mcuBlocks := aCollection.
	self resetSampleStream
]

{ #category : #accessing }
JPEGColorComponent >> mcuWidth: mw mcuHeight: mh dctSize: ds [ 
	mcuWidth := mw.
	mcuHeight := mh.
	dctSize := ds.
	hSampleFactor := mcuWidth // widthInBlocks.
	vSampleFactor := mcuHeight // heightInBlocks
]

{ #category : #'sample streaming' }
JPEGColorComponent >> nextSample [
	| dx dy blockIndex sampleIndex sample |
	dx := currentX // hSampleFactor.
	dy := currentY // vSampleFactor.
	blockIndex := dy // dctSize * widthInBlocks + (dx // dctSize) + 1.
	sampleIndex := dy \\ dctSize * dctSize + (dx \\ dctSize) + 1.
	sample := (mcuBlocks at: blockIndex) at: sampleIndex.
	currentX := currentX + 1.
	currentX < (mcuWidth * dctSize) ifFalse: 
		[ currentX := 0.
		currentY := currentY + 1 ].
	^ sample
]

{ #category : #accessing }
JPEGColorComponent >> priorDCValue: aNumber [ 
	priorDCValue := aNumber
]

{ #category : #accessing }
JPEGColorComponent >> qTableIndex [
	^qTableIndex
]

{ #category : #accessing }
JPEGColorComponent >> qTableIndex: anInteger [ 
	qTableIndex := anInteger
]

{ #category : #'sample streaming' }
JPEGColorComponent >> resetSampleStream [
	currentX := 0.
	currentY := 0
]

{ #category : #accessing }
JPEGColorComponent >> totalMcuBlocks [

	^ heightInBlocks * widthInBlocks
]

{ #category : #accessing }
JPEGColorComponent >> updateDCValue: aNumber [ 
	priorDCValue := priorDCValue + aNumber.
	^ priorDCValue
]

{ #category : #accessing }
JPEGColorComponent >> widthInBlocks [

	^widthInBlocks
]

{ #category : #accessing }
JPEGColorComponent >> widthInBlocks: anInteger [ 
	widthInBlocks := anInteger
]
