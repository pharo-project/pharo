private
classNamed: aClassName do: aBlock1 ifNone: aBlock2
	^ (self classNamed: aClassName)
		ifNotNil: aBlock1
		ifNil: aBlock2