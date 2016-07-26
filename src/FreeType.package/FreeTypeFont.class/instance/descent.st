measuring
descent
	^cachedDescent ifNil:[ 
		cachedDescent := ((self face descender * self pixelSize // self face unitsPerEm) negated) ]