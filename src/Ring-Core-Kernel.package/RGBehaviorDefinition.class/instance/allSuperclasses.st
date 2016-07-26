accessing class hierarchy
allSuperclasses
	"Answer an OrderedCollection of the receiver's and the receiver's  
	ancestor's superclasses"
	"Is implementation of Behavior more efficient?"
	
	^self annotationNamed: self class allSuperclassesKey
		ifAbsentPut:[ | supers sprClass |
			supers := OrderedCollection new.
			sprClass := self superclass.
			[sprClass notNil] whileTrue: 
				[supers add: sprClass.
				sprClass := sprClass superclass].
			supers ]