initialization
compactAllInstances

	| instancesToExchange newInstances |
	instancesToExchange := Array streamContents: [ :oldStream |
		newInstances := Array streamContents: [ :newStream |
			self allInstances do: [ :each |
				| newInstance |
				newInstance := each compactWithoutBecome.
				newInstance capacity = each capacity 
					ifTrue: [ each copyFrom: newInstance ]
					ifFalse: [
						oldStream nextPut: each.
						newStream nextPut: newInstance ] ] ] ].
	instancesToExchange elementsForwardIdentityTo: newInstances