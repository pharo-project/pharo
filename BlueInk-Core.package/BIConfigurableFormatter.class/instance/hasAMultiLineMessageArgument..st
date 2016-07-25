private
hasAMultiLineMessageArgument: anArgumentsCollection
	^ anArgumentsCollection
		anySatisfy: [ :each | 
			self isInCascadeNode
				ifTrue: [ self indent: 0 around: [ self willBeMultiline: each ] ]
				ifFalse: [ 
					self
						indent: self indentsForKeywords + 1
						around: [ self willBeMultiline: each ] ] ]