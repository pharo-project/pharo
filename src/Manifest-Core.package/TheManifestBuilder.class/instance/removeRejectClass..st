adding/removing
removeRejectClass: aClass

	|  classes |
	classes :=  self manifest rejectClasses asOrderedCollection.
	classes := classes reject: [ :each | 
			((self browsedEnvironment at: each first first) manifestReadOn:  (each first at: 2)) =  aClass asRingDefinition ].	
	self compileSelector: (self class rejectClassesTag) returnValue:  (self buildArrayString: classes)
	