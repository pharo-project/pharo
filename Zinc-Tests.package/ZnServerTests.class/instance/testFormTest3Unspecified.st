testing
testFormTest3Unspecified
	"Simulate what most browsers seem to do: upload text files utf-8 encoded 
	without specifying the charset in the mime type of the mime part"
	
	| inputs client part entity |
	inputs := #( ('foo.txt' 'this is foo!') ('français.txt' 'Je te dis: élèves français') ('Ελλάδα.txt' 'Ελλάδα is it!') ).
	self withServerDo: [ :server |
		server debugMode: true.
		(client := ZnClient new)
			url: server localUrl;
			addPathSegment: 'form-test-3'.
		inputs do: [ :each  |
			entity := ZnEntity 
				with: each second utf8Encoded 
				type: ZnMimeType textPlain clearCharSet.
			part := ZnMimePart fieldName: 'file' fileName: each first entity: entity.
			client
				resetEntity;
				addPart: part;
				post.
			self assert: client isSuccess.
			self assert: (client contents includesSubstring: each first).
			self assert: (client contents includesSubstring: each second) ] ]