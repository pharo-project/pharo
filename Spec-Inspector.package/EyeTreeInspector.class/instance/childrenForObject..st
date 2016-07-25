api
childrenForObject: anObject
	^ (EyeInspector inspector: anObject) elements 
			select: [ :element | element shouldShowInTree ]