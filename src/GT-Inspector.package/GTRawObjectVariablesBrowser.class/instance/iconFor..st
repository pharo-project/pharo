private building
iconFor: anObject
		
	^ [ (anObject gtInspectorIcon)
		ifNil: [ anObject class systemIcon ] ]
			on: Error do: [ :error | self iconFor: anObject error: error ]