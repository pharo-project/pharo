Extension { #name : #TClass }

{ #category : #'*Nautilus' }
TClass >> dropIn: aRPackage forNautilusUI: aNautilusUI [
	aRPackage addClass: self theNonMetaClass.
	ActiveHand shiftPressed
		ifTrue: [ aNautilusUI selectedClass: nil.
			aNautilusUI updateClassView ]
		ifFalse: [ aNautilusUI selectedPackage: aRPackage.
			aNautilusUI selectedClass: self.
			aNautilusUI updatePackageGroupAndClassList ]
]
