*Reflectivity
removeLink: aMetaLink
	self links remove: aMetaLink.
	self usingMethods do: [:method | method removeLink: aMetaLink].
	self links ifEmpty: [ self removeProperty: #links ].
	self clearReflectivityAnnotations.