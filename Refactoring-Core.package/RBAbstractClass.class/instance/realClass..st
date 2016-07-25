accessing
realClass: aClass 
	realClass := aClass.
	superclass isNil ifTrue: [superclass := LookupSuperclass]