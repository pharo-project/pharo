installing
unload
	"Unload me. If I represent an override method, then restore the overriden version. If not, just remove the selector from the class."

	| previousVersion |
	self isOverrideMethod ifTrue: [ previousVersion := self overridenMethodOrNil ].

	previousVersion
		ifNil: [ 
			self actualClass ifNotNil: [ :class | 
				self removeSelector: selector fromClass: class ] ]
		ifNotNil: [ previousVersion fileIn ]