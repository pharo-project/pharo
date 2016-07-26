accessing
referenceDependenciesFrom: aClass

	^ (self dependenciesFrom: aClass) select: [ :each | each isReferenceDependency ]