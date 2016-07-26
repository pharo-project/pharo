private
isBinary
	binary isNil
		ifTrue: [ [ binary := stream isBinary ] on: MessageNotUnderstood do: [ :err | binary := false ] ].
	^ binary