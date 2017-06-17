private
classFor: aType
	^ self subclasses detect: [ :class | class isType: aType ]