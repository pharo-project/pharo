private
filenameFor: anExample
	^ ((('gtExamplesFailure_', anExample method gtDisplayString		 
			copyReplaceAll: '>' with: '_') 
			copyReplaceAll: ':' with: '.') 
			copyReplaceAll: ' ' with: '') 
			copyReplaceAll: '#' with: ''