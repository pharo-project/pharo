testing
testFormTest2
	| inputs client |
	inputs := #( '1+2=3 & 2+1=3' 'single ''' 'double " - cool ?' 'élèves français' 'Ελλάδα' ).
	self withServerDo: [ :server |
		(client := ZnClient new)
			url: server localUrl;
			addPathSegment: 'form-test-2'.
		inputs do: [ :each  |
			client
				formAt: #input put: each;
				post.
			self assert: client isSuccess.
			self assert: (client contents includesSubstring: each) ] ]