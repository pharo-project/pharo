converting
asLiteralIn: env
	^ (env newClass name = className and: [ env newSelector = selector ])
		ifTrue: [ env newMethod ]
		ifFalse: [ (env classNamed: className) >> selector ]