protocol
addDependency: aDependency
	(dependencies includes: aDependency) 
		ifFalse: [ [ aDependency parent: self ] on: MessageNotUnderstood do: [ "ignore" ].
				 dependencies add: aDependency ] 