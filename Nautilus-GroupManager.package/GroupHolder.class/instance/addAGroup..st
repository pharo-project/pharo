adding
addAGroup: aGroup

	(self includesAGroupNamed: aGroup name)
		ifTrue: [ 
			self openError: aGroup name.
			^ nil].
	self groups add: aGroup.
	GroupAnnouncer uniqueInstance announce: ( AGroupHasBeenAdded group: aGroup into: self ).