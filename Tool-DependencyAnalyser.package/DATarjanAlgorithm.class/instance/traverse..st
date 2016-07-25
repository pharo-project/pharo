private
traverse: aPDPackage
	| aRelation |
	aPDPackage tarjanIndex: runningIndex.
	aPDPackage tarjanLowLink: runningIndex.
	runningIndex := runningIndex + 1.
	self push: aPDPackage.
	self traverseChildrenOf: aPDPackage.
	(self isRootNode: aPDPackage)
		ifTrue: [ 
			aRelation := self startNewSCC.
			self popStackAndUpdateCurrentSCC: aRelation until: aPDPackage ]