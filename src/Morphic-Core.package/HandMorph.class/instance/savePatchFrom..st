drawing
savePatchFrom: aCanvas 
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	| damageRect myBnds |
	damageRect := myBnds := self fullBounds.
	savedPatch ifNotNil: 
			[damageRect := myBnds merge: (savedPatch offset extent: savedPatch extent)].
	(savedPatch isNil or: [savedPatch extent ~= myBnds extent]) 
		ifTrue: 
			["allocate new patch form if needed"

			savedPatch := aCanvas form allocateForm: myBnds extent].
	aCanvas contentsOfArea: (myBnds translateBy: aCanvas origin)
		into: savedPatch.
	savedPatch offset: myBnds topLeft.
	^damageRect