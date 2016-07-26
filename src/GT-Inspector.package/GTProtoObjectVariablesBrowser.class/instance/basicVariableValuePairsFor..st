private building
basicVariableValuePairsFor: anObject

	^ (anObject class allInstVarNames 
			collectWithIndex: [ :ivName :index | 
				ivName -> (thisContext object: anObject instVarAt: index) ]) 
					sort
			