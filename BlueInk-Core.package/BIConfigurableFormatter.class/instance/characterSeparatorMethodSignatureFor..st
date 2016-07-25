private
characterSeparatorMethodSignatureFor: aMethodNode
	^ [ 
	(self needsMethodSignatureOnMultipleLinesFor: aMethodNode)
		ifTrue: [ self newLine ]
		ifFalse: [ self space ] ]