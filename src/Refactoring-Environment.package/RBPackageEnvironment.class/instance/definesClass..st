testing
definesClass: aClass
	^ (super definesClass: aClass) and: [ self packages anySatisfy: [ :package | package includesClass: aClass ] ]