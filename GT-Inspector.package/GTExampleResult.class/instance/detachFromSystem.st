private
detachFromSystem
	example ifNotNil: [ 
		example result: nil.
		GTExampleNautilus methodChanged: example method ].
	example := returnValue := expectedError := unexpectedError := nil.