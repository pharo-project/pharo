initialization
initialize
	super initialize.
	methods := OrderedCollection new.
	SystemAnnouncer uniqueInstance weak when: MethodAdded send: #methodAdded: to: self.
	SystemAnnouncer uniqueInstance weak when: MethodModified send: #methodModified: to: self