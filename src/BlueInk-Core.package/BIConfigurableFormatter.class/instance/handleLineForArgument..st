private
handleLineForArgument: anArgument
	(self keepBlockInMessage and: [ anArgument isBlock ])
		ifTrue: [ (self willBeMultiline: anArgument)
				ifTrue: [ (self formattedSourceFor: anArgument) lines first size > self maxLineLength
						ifTrue: [ self newLine ]
						ifFalse: [ self space ] ]
				ifFalse: [ anArgument sourceInterval size > self maxLineLength
						ifTrue: [ self newLine ]
						ifFalse: [ self space ] ] ]
		ifFalse: [ ((self willBeMultiline: anArgument)
				or: [ anArgument sourceInterval size > self maxLineLength ])
				ifTrue: [ self isInCascadeNode
						ifTrue: [ codeStream nextPut: Character cr.
							self indent - 1 timesRepeat: [ codeStream nextPutAll: self indentString ] ]
						ifFalse: [ self newLine ] ]
				ifFalse: [ self space ] ].
	self visitNode: anArgument