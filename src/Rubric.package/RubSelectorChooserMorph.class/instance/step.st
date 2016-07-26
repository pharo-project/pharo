stepping and presenter
step
	[requestorPos 
		ifNil: [requestorPos := requestor owner position]
		ifNotNil: [requestorPos ~= requestor owner position
			ifTrue: [^ self close]].
	(requestor isNil or: [requestor isInWorld not])
		ifTrue: [^ self close].
	requestor editor selectorChooser ~= self 
		ifTrue: [self close]
	] on: Error do: [self delete]
