tests
testIndirectCircularHierarchyError
	"Tests an error is raised when trying to create a heirarchy A<-B<-A"
	
	| classA classB |
	classA := self make: [ :builder |
			builder 
				superclass: Object;
				name: self aClassName ].

	classB := self make: [ :builder |
			builder 
				superclass: classA;
				name: self anotherClassName ].

	self should: [ 
		self make: [ :builder | 
			builder 
				superclass: classB;
				name: self aClassName ] ]
		raise: CircularHierarchyError 