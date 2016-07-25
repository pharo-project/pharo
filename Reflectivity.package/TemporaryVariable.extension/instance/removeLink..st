*Reflectivity
removeLink: aMetaLink
	self usingMethods do: [:mth | mth removeLink: aMetaLink].
	self links remove: aMetaLink.
	self links ifEmpty: [ self removeProperty: #links ].
	self clearReflectivityAnnotations.