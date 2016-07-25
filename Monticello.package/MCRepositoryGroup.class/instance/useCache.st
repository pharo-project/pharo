accessing
useCache
	^ useCache ifNil: [
		useCache := true
	]