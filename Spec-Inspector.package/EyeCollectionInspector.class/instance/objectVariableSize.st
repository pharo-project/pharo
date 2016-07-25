accessing
objectVariableSize
	^ [ self object size ] on: Error do: [ self object basicSize ]