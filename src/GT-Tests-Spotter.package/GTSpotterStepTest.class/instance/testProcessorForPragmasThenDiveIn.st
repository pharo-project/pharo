tests
testProcessorForPragmasThenDiveIn
	self testProcessorForPragmas.
	
	self assertDiveIn.
	
	self assertSearch: ''.

	self assertProcessorByTitle: 'Pragmas'.
	self assertProcessorNotEmpty.
	self assertProcessorIncludesAll: (self pragmas: #spotterOrder:).
		
	self assertProcessorByTitle: 'Users'.
	self assertProcessorNotEmpty.
	self assertProcessorIncludesAll: (self users: #spotterOrder:).
	
	self assertSearch: 'spotterForClassesFor:'.
	
	self assertProcessorByTitle: 'Pragmas'.
	self assertProcessorNotEmpty.
	self assertProcessorIncludes: (self pragma: #spotterOrder: of: (GTSpotter >> #spotterForClassesFor:)).
	
	self assertSearch: 'extensionsPragma'.
	
	self assertProcessorByTitle: 'Users'.
	self assertProcessorNotEmpty.
	self assertProcessorIncludes: (GTSpotter class >> #extensionsPragma).
	
	self assertSearch: 'spotterForF'.
	
	self assertProcessorByTitle: 'Pragmas'.
	self assertProcessorNotEmpty.
	self assertProcessorIncludes: (self pragma: #spotterOrder: of: (GTSpotter >> #spotterForFilesFor:)).
	self assertProcessorIncludes: (self pragma: #spotterOrder: of: (FileReference >> #spotterForFilesFor:)).
	
	self assertDiveOut.
	
	self assert: currentStep context text = 'spotterOrder:'