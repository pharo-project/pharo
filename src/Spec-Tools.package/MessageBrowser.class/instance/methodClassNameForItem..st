private
methodClassNameForItem: anItem
	^ anItem methodClass ifNotNil: [ :class | class name ] ifNil: [ '' ]