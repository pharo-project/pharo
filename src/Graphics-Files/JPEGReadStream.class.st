"
Encapsulates huffman encoded access to JPEG data.

The following layout is fixed for the JPEG primitives to work:

	collection	<ByteArray | String>
	position		<SmallInteger>
	readLimit	<SmallInteger>
	bitBuffer	<SmallInteger>
	bitsInBuffer	<SmallInteger>
"
Class {
	#name : #JPEGReadStream,
	#superclass : #ReadStream,
	#instVars : [
		'bitBuffer',
		'bitsInBuffer'
	],
	#classVars : [
		'MaxBits'
	],
	#category : #'Graphics-Files'
}

{ #category : #initialization }
JPEGReadStream class >> initialize [
	"JPEGReadStream initialize"
	MaxBits := 16
]

{ #category : #'huffman trees' }
JPEGReadStream >> buildLookupTable: values counts: counts [ 
	| min max |
	min := max := nil.
	1 
		to: counts size
		do: 
			[ :i | 
			(counts at: i) = 0 ifFalse: 
				[ min ifNil: [ min := i - 1 ].
				max := i ] ].
	^ self 
		createHuffmanTables: values
		counts: {  0  } , counts
		from: min + 1
		to: max
]

{ #category : #'huffman trees' }
JPEGReadStream >> createHuffmanTables: values counts: counts from: minBits to: maxBits [ 
	"Create the actual tables"
	| table tableStart tableSize tableEnd valueIndex tableStack numValues deltaBits maxEntries lastTable lastTableStart tableIndex lastTableIndex |
	table := WordArray new: ((4 bitShift: minBits) max: 16).

	"Create the first entry - this is a dummy.
	It gives us information about how many bits to fetch initially."
	table 
		at: 1
		put: (minBits bitShift: 24) + 2.	"First actual table starts at index 2"

	"Create the first table from scratch."
	tableStart := 2.	"See above"
	tableSize := 1 bitShift: minBits.
	tableEnd := tableStart + tableSize.
	"Store the terminal symbols"
	valueIndex := counts at: minBits + 1.
	tableIndex := 0.
	1 
		to: valueIndex
		do: 
			[ :i | 
			table 
				at: tableStart + tableIndex
				put: (values at: i).
			tableIndex := tableIndex + 1 ].
	"Fill up remaining entries with invalid entries"
	tableStack := OrderedCollection new: 10.	"Should be more than enough"
	tableStack addLast: (Array 
			with: minBits
			with: tableStart
			with: tableIndex
			with: minBits
			with: tableSize - valueIndex).	"Number of bits (e.g., depth) for this table"	"Start of table"	"Next index in table"	"Number of delta bits encoded in table"	"Entries remaining in table"
	"Go to next value index"
	valueIndex := valueIndex + 1.
	"Walk over remaining bit lengths and create new subtables"
	minBits + 1 
		to: maxBits
		do: 
			[ :bits | 
			numValues := counts at: bits + 1.
			[ numValues > 0 ] whileTrue: 
				[ "Create a new subtable"
				lastTable := tableStack last.
				lastTableStart := lastTable at: 2.
				lastTableIndex := lastTable at: 3.
				deltaBits := bits - (lastTable at: 1).
				"Make up a table of deltaBits size"
				tableSize := 1 bitShift: deltaBits.
				tableStart := tableEnd.
				tableEnd := tableEnd + tableSize.
				[ tableEnd > table size ] whileTrue: [ table := self growHuffmanTable: table ].
				"Connect to last table"
				[ (table at: lastTableStart + lastTableIndex) = 0 ] assert.	"Entry must be unused"
				table 
					at: lastTableStart + lastTableIndex
					put: (deltaBits bitShift: 24) + tableStart.
				lastTable 
					at: 3
					put: lastTableIndex + 1.
				lastTable 
					at: 5
					put: (lastTable at: 5) - 1.
				[ (lastTable at: 5) >= 0 ] assert.	"Don't exceed tableSize"
				"Store terminal values"
				maxEntries := numValues min: tableSize.
				tableIndex := 0.
				1 
					to: maxEntries
					do: 
						[ :i | 
						table 
							at: tableStart + tableIndex
							put: (values at: valueIndex).
						valueIndex := valueIndex + 1.
						numValues := numValues - 1.
						tableIndex := tableIndex + 1 ].
				"Check if we have filled up the current table completely"
				maxEntries = tableSize 
					ifTrue: 
						[ "Table has been filled. Back up to the last table with space left."
						[ tableStack isEmpty not and: [ (tableStack last at: 5) = 0 ] ] whileTrue: [ tableStack removeLast ] ]
					ifFalse: 
						[ "Table not yet filled. Put it back on the stack."
						tableStack addLast: (Array 
								with: bits
								with: tableStart
								with: tableIndex
								with: deltaBits
								with: tableSize - maxEntries)	"Nr. of bits in this table"	"Start of table"	"Index in table"	"delta bits of table"	"Unused entries in table" ] ] ].
	^ table 
		copyFrom: 1
		to: tableEnd - 1
]

{ #category : #'huffman trees' }
JPEGReadStream >> decodeValueFrom: table [ 
	"Decode the next value in the receiver using the given huffman table."
	| bits bitsNeeded tableIndex value |
	bitsNeeded := (table at: 1) bitShift: -24.	"Initial bits needed"
	tableIndex := 2.	"First real table"
	
	[ bits := self getBits: bitsNeeded.	"Get bits"
	value := table at: tableIndex + bits.	"Lookup entry in table"
	(value bitAnd: 1056964608) = 0	"Check if it is a non-leaf node" ] whileFalse: 
		[ "Fetch sub table"
		tableIndex := value bitAnd: 65535.	"Table offset in low 16 bit"
		bitsNeeded := (value bitShift: -24) bitAnd: 255.	"Additional bits in high 8 bit"
		bitsNeeded > MaxBits ifTrue: [ ^ self error: 'Invalid huffman table entry' ] ].
	^ value
]

{ #category : #accessing }
JPEGReadStream >> fillBuffer [
	| byte |
	[ bitsInBuffer <= 16 ] whileTrue: 
		[ byte := self next.
		(byte = 255 and: [ (self peekFor: 0) not ]) ifTrue: 
			[ self position: self position - 1.
			^ 0 ].
		bitBuffer := (bitBuffer bitShift: 8) bitOr: byte.
		bitsInBuffer := bitsInBuffer + 8 ].
	^ bitsInBuffer
]

{ #category : #accessing }
JPEGReadStream >> getBits: requestedBits [ 
	| value |
	requestedBits > bitsInBuffer ifTrue: 
		[ self fillBuffer.
		requestedBits > bitsInBuffer ifTrue: [ self error: 'not enough bits available to decode' ] ].
	value := bitBuffer bitShift: requestedBits - bitsInBuffer.
	bitBuffer := bitBuffer bitAnd: (1 bitShift: bitsInBuffer - requestedBits) - 1.
	bitsInBuffer := bitsInBuffer - requestedBits.
	^ value
]

{ #category : #'huffman trees' }
JPEGReadStream >> growHuffmanTable: table [ 
	| newTable |
	newTable := table species new: table size * 2.
	newTable 
		replaceFrom: 1
		to: table size
		with: table
		startingAt: 1.
	^ newTable
]

{ #category : #accessing }
JPEGReadStream >> nextByte [
	^self next asInteger
]

{ #category : #accessing }
JPEGReadStream >> nextBytes: n [
	^(self next: n) asByteArray
]

{ #category : #initialization }
JPEGReadStream >> reset [
	super reset.
	self resetBitBuffer
]

{ #category : #accessing }
JPEGReadStream >> resetBitBuffer [
	bitBuffer := 0.
	bitsInBuffer := 0
]
