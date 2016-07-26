*Reflectivity
link: aMetaLink
	| methods |
	(aMetaLink checkForCompatibilityWith: self) ifFalse: [ self error: 'link requests reification that can not be provided by this node' ].
	
	methods := self usingMethods.
	(self propertyAt: #links ifAbsentPut: [ IdentitySet new ]) add: aMetaLink.
	aMetaLink installOnVariable: self.
	methods
		do: [ :method |  
			method
				createTwin;
				invalidate.
			method installLink: aMetaLink ]