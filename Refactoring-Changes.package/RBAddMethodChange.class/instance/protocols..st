initialize-release
protocols: aCollectionOrString
	protocols := aCollectionOrString isString
		ifTrue: [ 	
			aCollectionOrString = Protocol unclassified 
				ifTrue: [ protocols := #(accessing) ]
				ifFalse: [ Array with: aCollectionOrString ] ]
		ifFalse: [ aCollectionOrString ].
		
