importing
importFor: aRequestor logSource: logSource
	(self existsBehavior)
		ifFalse: [ self error: ('Cannot change organization of unexistent behavior {1}' format: { behaviorName asString } ) ]. 
		
	self targetClass organization changeFromString: contents