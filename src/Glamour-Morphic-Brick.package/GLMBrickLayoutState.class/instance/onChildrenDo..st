doing
onChildrenDo: aBlock

	^ self
		on: #isOnChildren
		do: aBlock
		ifAbsent: [ GLMBrickLayoutChildrenState new ]