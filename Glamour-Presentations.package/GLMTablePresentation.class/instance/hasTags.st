testing
hasTags

	^ self columns anySatisfy: [ :aColumn | aColumn hasTags ]