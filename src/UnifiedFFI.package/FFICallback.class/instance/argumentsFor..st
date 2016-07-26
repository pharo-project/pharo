private
argumentsFor: stackPointer
	| index |
	index := 1.
	^ functionSpec arguments 
		collect: [ :each | | value |
			value := each callbackValueFor: stackPointer at: index.
			index := index + each typeSize.
			value ].	