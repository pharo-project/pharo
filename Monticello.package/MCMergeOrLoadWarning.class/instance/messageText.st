accessing
messageText
	| packageNames |
	
	packageNames := versions collect: [ :ea|  ea package name ].
		
	^ String streamContents: [:s |
		s nextPutAll: 'You are about to load new versions of the following packages'; cr;
			nextPutAll: 'that have unsaved changes in the image:'; cr.
			
		packageNames sort do: [:name |
			s cr; space; space; nextPutAll: name].
		
		s cr cr
			nextPutAll: 'If you continue, you will lose these changes:']