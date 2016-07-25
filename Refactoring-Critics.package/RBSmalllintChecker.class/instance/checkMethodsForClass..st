private
checkMethodsForClass: aClass
	^ environment
		selectorsForClass: aClass
		do: [ :each | 
			rule checkMethod: aClass>>each.
			methodBlock value ]