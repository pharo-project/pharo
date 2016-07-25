actions
blockCode
	^ block
		ifNil: ['']
		ifNotNil: [ blockSource ifNil: [ blockSource := block asString ]]