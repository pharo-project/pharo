actions
inspectSubInstances
	"Inspect all instances of the selected class and all its subclasses"

	self currentClassOrMetaClass ifNotNil: [ :aClass |
		aClass theNonMetaClass inspectSubInstances ]