class initialization
initializeTagConstants
	" Unicode initializeTagConstants "
	(self classPool keys
		select: [:sym | sym size = 2 
						and: [sym first isUppercase and: [sym last isLowercase]]]) asSortedCollection
		inject: 1
		into: [:index :sym | sym = #Cn
				ifTrue: [self classPool at: sym put: 0. index]
				ifFalse: [self classPool at: sym put: index. index + 1]]