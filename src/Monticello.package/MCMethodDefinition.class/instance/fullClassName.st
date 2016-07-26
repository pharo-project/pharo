printing
fullClassName
	"Using #class selector for classes for backwards compatibility"

	^ self classIsMeta
		ifFalse: [self className]
		ifTrue: [
			(self actualClass isNil or: [ self actualClass isTrait ])
				ifFalse: [self className, ' class']
				ifTrue: [self className, ' classSide']]