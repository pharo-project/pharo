doing
onMyselfDo: aBlock

	^ self
		on: #isOnMyself
		do: aBlock
		ifAbsent: [ GLMBrickLayoutMyselfState new ]