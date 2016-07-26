tests
testDirectCircularHierarchyError
	"Tests an error is raised when trying to create a heirarchy A<-A"
	
	| classA |
	classA := self make: [ :builder |
			builder 
				superclass: Object;
				name: self aClassName ].

	self should: [ 
		self make: [ :builder | 
			builder 
				superclass: classA;
				name: self aClassName ] ]
		raise: CircularHierarchyError 