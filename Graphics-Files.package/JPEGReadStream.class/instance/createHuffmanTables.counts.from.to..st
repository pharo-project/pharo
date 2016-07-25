huffman trees
createHuffmanTables: values counts: counts from: minBits to: maxBits 
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