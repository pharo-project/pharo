private-receiver-guessing
checkImpossibleReceiver
	^ self isSelectorsAndVariables
		ifTrue: [self configureSelectorsAndVariables]
		ifFalse: [self isVariablesOnly
				ifTrue: [self configureVariablesOnly]
				ifFalse: [self isSelectorsOnly
						and: [self configureSelectorsOnly]]]. 