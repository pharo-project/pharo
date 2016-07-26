accessing
superclassName

	^ superclassName ifNil: [ superclassName := 'Object' ]
