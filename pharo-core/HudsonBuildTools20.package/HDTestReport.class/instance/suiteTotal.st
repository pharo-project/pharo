accessing
suiteTotal
	^ suite 
		ifNotNil: [ suite tests size ]
		ifNil: [ 0 ]