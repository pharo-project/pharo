as yet unclassified
fitTargetBoundsInOwner: aRect
	"Reset the target bounds if required to
	ensure that the owner's submorphs fit within the owner's
	exisiting bounds when layed out."

	|ownerMinExt targetOwner|
	targetOwner := self target owner ifNil: [^self].
	ownerMinExt :=  targetOwner minExtent.
	ownerMinExt x > self target owner width
		ifTrue: [self edgeName = #left
				ifTrue: [self target bounds: (aRect left + (ownerMinExt x - targetOwner width) @ aRect top extent: (aRect width - (ownerMinExt x - targetOwner width)) @ aRect height)]
				ifFalse: [self target bounds: (aRect origin extent: (aRect width - (ownerMinExt x - targetOwner width)) @ aRect height)]].
	ownerMinExt y > self target owner height
		ifTrue: [self edgeName = #top
				ifTrue: [self target bounds: (aRect left @ (aRect top + (ownerMinExt y - targetOwner height)) extent: aRect width @ (aRect height - (ownerMinExt y - targetOwner height)))]
				ifFalse: [self target bounds: (aRect origin extent: aRect width @ (aRect height - (ownerMinExt y - targetOwner height)))]]
		
	