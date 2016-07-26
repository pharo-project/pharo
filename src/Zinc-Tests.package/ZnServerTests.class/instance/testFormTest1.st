testing
testFormTest1
	| inputs client |
	inputs := #( '1+2=3 & 2+1=3' 'single ''' 'double " - cool ?' 'élèves français' 'Ελλάδα' ).
	self withServerDo: [ :server |
		(client := ZnClient new)
			url: server localUrl;
			addPathSegment: 'form-test-1'.
		inputs do: [ :each  |
			client
				queryAt: #input put: each;
				get.
			self assert: client isSuccess.
			self assert: (client contents includesSubstring: each) ] ]