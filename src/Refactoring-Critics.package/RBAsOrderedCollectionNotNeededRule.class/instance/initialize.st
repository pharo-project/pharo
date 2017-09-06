initialization
initialize
	super initialize.
	#('addAll:' 'removeAll:' 'includesAll:' 'copyWithoutAll:')
		do: [ :collectionMessage | 
			#('asArray' 'asOrderedCollection' 'asSortedCollection')
				do: [ :conversionMessage | 
					| baseString |
					baseString := '``@receiver ' , collectionMessage , ' ``@arg '.
					self replace: baseString , conversionMessage with: baseString ] ]