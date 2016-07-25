private
superclass: aRBClass 
	self superclass notNil ifTrue: [self superclass removeSubclass: self].
	superclass := aRBClass.
	superclass notNil ifTrue: [superclass addSubclass: self].