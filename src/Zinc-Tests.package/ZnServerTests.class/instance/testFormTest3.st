testing
testFormTest3
	| inputs client part |
	inputs := #( ('foo.txt' 'this is foo!') ('français.txt' 'Je te dis: élèves français') ('Ελλάδα.txt' 'Ελλάδα is it!') ).
	self withServerDo: [ :server |
		server debugMode: true.
		(client := ZnClient new)
			url: server localUrl;
			addPathSegment: 'form-test-3'.
		inputs do: [ :each  |
			part := ZnMimePart 
				fieldName: 'file' 
				fileName: each first 
				entity: (ZnEntity with: each second).
			client
				resetEntity;
				addPart: part;
				post.
			self assert: client isSuccess.
			self assert: (client contents includesSubstring: each first).
			self assert: (client contents includesSubstring: each second) ] ]