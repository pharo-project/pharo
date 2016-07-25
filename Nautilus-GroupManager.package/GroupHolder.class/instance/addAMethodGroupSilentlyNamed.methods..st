adding
addAMethodGroupSilentlyNamed: entry methods: aCollectionOfMethods
	(self includesAGroupNamed: entry)
		ifTrue: [ ^ nil ]
		ifFalse: [ 
			| group |
			group := self groups add: (MethodGroup named: entry methods: aCollectionOfMethods).
			^ group ]