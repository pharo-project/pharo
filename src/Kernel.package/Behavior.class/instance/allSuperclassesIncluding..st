accessing class hierarchy
allSuperclassesIncluding: aClass
	"Answer an OrderedCollection of the receiver's and the receiver's  ancestor's superclasses
	up to aClass included. If aClass is not part of the receiver's superclass, returns up to the root."
	| temp |
	self class == ProtoObject class ifTrue: [ ^ OrderedCollection new].
	^ self superclass == aClass
		ifTrue: [ OrderedCollection with: aClass]
		ifFalse: [temp := self superclass allSuperclassesIncluding: aClass.
			temp addFirst: self superclass.
			temp]