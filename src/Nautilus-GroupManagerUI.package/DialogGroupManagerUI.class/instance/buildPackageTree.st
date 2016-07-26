item creation
buildPackageTree
	| morph |
	self groupManager announcer when: MorphTreeSelectionChanged send: #itemSelected: to: self.
	self groupManager ghostString: 'Search ...'.
	self groupManager rootItems: RPackageOrganizer default packages sorted.
	morph := self groupManager defaultMorphIn: World.
	^ morph