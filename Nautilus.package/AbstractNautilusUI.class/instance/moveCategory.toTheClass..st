private
moveCategory: anObject toTheClass: aClass
	
	| originClass methods |
	originClass := self selectedClass.
	methods := originClass methodsInProtocol: anObject.
	methods do: [:meth | self moveMethod: meth toTheClass: aClass autoRemove: true ].
	
	(originClass methodsInProtocol: anObject)
		ifEmpty: [ 
			originClass removeProtocol: anObject.
			self selectedCategory: nil ].