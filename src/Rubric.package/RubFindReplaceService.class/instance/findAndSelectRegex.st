private
findAndSelectRegex
	^ self findRegex ifNotNil: [ self findAndSelect ]