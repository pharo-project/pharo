measuring
height

	^cachedHeight ifNil:[
		cachedHeight := (self face height * self pixelSize / self face unitsPerEm) ceiling ]