initialize-release
initializeGrayToIndexMap
	"Build an array of gray values available in the 8-bit colormap. This array is indexed by a gray level between black (1) and white (256) and returns the pixel value for the corresponding gray level."
	"Note: This method must be called after initializeIndexedColors, since it uses IndexedColors."
	"Color initializeGrayToIndexMap"
	"record the level and index of each gray in the 8-bit color table"
	| grayLevels grayIndices c distToClosest dist indexOfClosest |
	grayLevels := OrderedCollection new.
	grayIndices := OrderedCollection new.
	"Note: skip the first entry, which is reserved for transparent"
	2 
		to: IndexedColors size
		do: 
			[ :i | 
			c := IndexedColors at: i.
			c saturation = 0.0 ifTrue: 
				[ "c is a gray"
				grayLevels add: c privateBlue >> 2.	"top 8 bits; R, G, and B are the same"
				grayIndices add: i - 1 ] ].	"pixel values are zero-based"
	grayLevels := grayLevels asArray.
	grayIndices := grayIndices asArray.

	"for each gray level in [0..255], select the closest match"
	GrayToIndexMap := ByteArray new: 256.
	0 
		to: 255
		do: 
			[ :level | 
			distToClosest := 10000.	"greater than distance to any real gray"
			1 
				to: grayLevels size
				do: 
					[ :i | 
					dist := (level - (grayLevels at: i)) abs.
					dist < distToClosest ifTrue: 
						[ distToClosest := dist.
						indexOfClosest := grayIndices at: i ] ].
			GrayToIndexMap 
				at: level + 1
				put: indexOfClosest ]