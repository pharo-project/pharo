actions browsing
inspectInstances
	"Inspect all instances of the selected class."

	self currentClassOrMetaClass ifNotNil: [ :aClass |
		aClass theNonMetaClass inspectAllInstances ]