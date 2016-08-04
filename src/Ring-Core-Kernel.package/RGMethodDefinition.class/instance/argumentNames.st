traits
argumentNames
	"Return an array with the argument names of the method's selector"

	self isActive
		ifTrue: [ ^ self compiledMethod argumentNames ].
	^ self ast ifNotNil:[:ast | ast arguments collect: #name ]