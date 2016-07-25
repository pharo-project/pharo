doing
onParentDo: aBlock

	^ self
		on: #isOnParent
		do: aBlock
		ifAbsent: [ GLMBrickLayoutParentState new ]