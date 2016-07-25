initialize-release
addClass: aName superclass: aClass subclasses: aCollection category: aSymbol 
	self className: aName.
	superclass := self classObjectFor: aClass.
	subclasses := aCollection collect: [:each | self classObjectFor: each].
	category := aSymbol