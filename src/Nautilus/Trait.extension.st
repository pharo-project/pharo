Extension { #name : #Trait }

{ #category : #'*Nautilus' }
Trait >> dropIn: aRPackage forNautilusUI: aNautilusUI [
	aRPackage addClass: self instanceSide.
	ActiveHand shiftPressed
		ifTrue: [ aNautilusUI selectedClass: nil.
			aNautilusUI updateClassView ]
		ifFalse: [ aNautilusUI selectedPackage: aRPackage.
			aNautilusUI selectedClass: self.
			aNautilusUI updatePackageGroupAndClassList ]
]
