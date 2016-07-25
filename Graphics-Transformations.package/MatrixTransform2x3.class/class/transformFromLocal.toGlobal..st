instance creation
transformFromLocal: localBounds toGlobal: globalBounds
	^((self withOffset: (globalBounds center)) composedWithLocal:
		(self withScale: (globalBounds extent / localBounds extent) asFloatPoint))
			composedWithLocal: (self withOffset: localBounds center negated)
"
	^(self identity)
		setScale: (globalBounds extent / localBounds extent) asFloatPoint;
		setOffset: localBounds center negated asFloatPoint;
		composedWithGlobal:(self withOffset: globalBounds center asFloatPoint)
"