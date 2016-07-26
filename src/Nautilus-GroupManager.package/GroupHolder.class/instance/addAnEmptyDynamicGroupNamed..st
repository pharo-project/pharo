adding
addAnEmptyDynamicGroupNamed: aName

	^ (self addAPackageGroupSilentlyNamed: aName packages: OrderedCollection new)
			isFillable: true;
			yourself