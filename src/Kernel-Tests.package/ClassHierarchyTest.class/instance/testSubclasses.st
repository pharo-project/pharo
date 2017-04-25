tests
testSubclasses
	" self class fixSubclasses "

	SystemNavigation new
		allClassesDo: [ :cls | 
			self
				assert: (cls superclass subclasses includes: cls)
				description: cls name , ' is not in ' , (cls superclass ifNotNil: [ :superclass | superclass name ] ifNil: [ 'nil' ]) , '''s subclasses' ]