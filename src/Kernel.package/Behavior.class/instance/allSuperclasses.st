accessing class hierarchy
allSuperclasses
	"Answer an OrderedCollection of the receiver's and the receiver's  
	ancestor's superclasses. The first element is the receiver's immediate  
	superclass, followed by its superclass; the last element is Object."
	| temp |
	^ self superclass == nil
		ifTrue: [ OrderedCollection new]
		ifFalse: [temp := self superclass allSuperclasses.
			temp addFirst: self superclass.
			temp]