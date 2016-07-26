private
verify
	self assert: allFailures isEmptyOrNil description: (String streamContents: [ :stream |
		allFailures do: [ :each | 
			stream nextPutAll: each gtDisplayString; cr ] ] )