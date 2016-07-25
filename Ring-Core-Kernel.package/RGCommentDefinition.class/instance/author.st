stamp values
author 

	^self annotationNamed: self class authorKey
		ifAbsentPut:[ self class parseAuthorAliasFrom: stamp ]