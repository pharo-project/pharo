versions
bootstrapVersionsExcept: aBlock

	^ self bootstrapVersionsSatisfying: [ :arg | (aBlock value: arg) not ]