*Reflectivity
link: aMetaLink
	(aMetaLink checkForCompatibilityWith: self) ifFalse: [ self error: 'link requests reification that can not be provided by this node' ].
	
	(self propertyAt: #links ifAbsentPut: [ IdentitySet new ]) add: aMetaLink.
	aMetaLink installOnVariable: self.
	method 
		createTwin;
		invalidate.
	method installLink: aMetaLink