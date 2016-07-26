accessing methods
extensionMethods

	^self methods select:[ :each | each isExtension ]