private
haltIfBlockWithCallingObject: aBlock

	| callingObject |
	callingObject := thisContext sender sender receiver.
	(aBlock cull: callingObject)
		ifTrue: [self signal]
		ifFalse: [^ self].