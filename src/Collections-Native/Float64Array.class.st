Class {
	#name : 'Float64Array',
	#superclass : 'ArrayedCollection',
	#type : 'DoubleWordLayout',
	#category : 'Collections-Native-Base',
	#package : 'Collections-Native',
	#tag : 'Base'
}

{ #category : 'accessing' }
Float64Array >> at64Bits: byteIndex [

	<primitive: 614>
	| elementIndex |
	elementIndex := byteIndex / 8 + 1.
	^ Float fromIEEE64BitWord: (self basicAt: elementIndex)
]

{ #category : 'accessing' }
Float64Array >> at64Bits: byteIndex put: aFloat [

	<primitive: 629>
	| elementIndex |
	elementIndex := byteIndex / 8 + 1.
	self basicAt: elementIndex put: aFloat asIEEE64BitWord.
	^ aFloat
]

{ #category : 'accessing' }
Float64Array >> at: anIndex [

	^ self at64Bits: anIndex - 1 * 8
]

{ #category : 'accessing' }
Float64Array >> at: anIndex put: aFloat [

	^ self at64Bits: anIndex - 1 * 8 put: aFloat
]
