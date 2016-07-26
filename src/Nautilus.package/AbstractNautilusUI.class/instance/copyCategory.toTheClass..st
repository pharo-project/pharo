private
copyCategory: anObject toTheClass: aClass
	
	| originClass methods |
	originClass := self selectedClass.
	methods := originClass methodsInProtocol: anObject.
	methods do: [:meth | self copyMethod: meth toTheClass: aClass ].