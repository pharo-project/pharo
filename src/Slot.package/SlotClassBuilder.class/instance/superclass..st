accessing
superclass: aSuperclass
	superclass := aSuperclass.
	superclass ifNotNil: [ self validateSuperclass: superclass ].