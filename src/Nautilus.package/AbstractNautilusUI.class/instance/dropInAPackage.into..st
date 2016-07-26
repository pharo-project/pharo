drag and drop
dropInAPackage: aCollection into: receiver
	aCollection
		do:
			[ :aClass | 
			receiver addClass: aClass theNonMetaClass.
			ActiveHand shiftPressed
				ifTrue: [ 
					self selectedClass: nil.
					self updateClassView ]
				ifFalse:
					[ 
					self selectedPackage: receiver.
					self selectedClass: aClass.
					self updatePackageGroupAndClassList ] ]