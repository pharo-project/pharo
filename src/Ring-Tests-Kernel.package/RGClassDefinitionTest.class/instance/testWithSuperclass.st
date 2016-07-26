testing
testWithSuperclass
	| newClass supClass  |
	
	supClass:= (RGClassDefinition named:  #Object)
						superclassName: #ProtoObject;
						yourself.

	self assert: (supClass hasSuperclass not).
	self assert: (supClass superclassName == #ProtoObject).   "kept as annotation"
	self assert: (supClass annotations size = 1).
	
	newClass := (RGClassDefinition named: #OrderedCollection) superclass: supClass.
	self assert: (newClass superclass = supClass).
	self assert: (newClass superclassName == #Object).
	self assert: (newClass withAllSuperclasses size = 2).
	self assert: (newClass allSuperclasses size = 1).
	
	self assert: (supClass subclasses size = 1).
	self assert: (supClass withAllSubclasses size = 2).
	self assert: (supClass allSubclasses size = 1).