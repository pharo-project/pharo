fonts and font indexes
fontIndexOfPointSize: desiredPointSize 
	"Returns an index in fontArray of the font with pointSize <= desiredPointSize"
	"Leading is not inluded in the comparison"
	| bestMatch bestIndex d |
	bestMatch := 9999.
	bestIndex := 1.
	1 
		to: fontArray size
		do: 
			[ :i | 
			d := desiredPointSize - (fontArray at: i) pointSize.
			d = 0 ifTrue: [ ^ i ].
			(d > 0 and: [ d < bestMatch ]) ifTrue: 
				[ bestIndex := i.
				bestMatch := d ] ].
	^ bestIndex