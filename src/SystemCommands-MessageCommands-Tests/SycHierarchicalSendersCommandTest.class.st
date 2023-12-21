Class {
	#name : 'SycHierarchicalSendersCommandTest',
	#superclass : 'TestCase',
	#category : 'SystemCommands-MessageCommands-Tests',
	#package : 'SystemCommands-MessageCommands-Tests'
}

{ #category : 'tests' }
SycHierarchicalSendersCommandTest >> testSearchInTheWholeHierarchy [

	| senders |
	senders := SycShowLocalSendersCommand new
			selectedClass: SycMockClassForHierarhicalSenderB;
			selectedMessage: (SycMockClassForHierarhicalSenderB >> #referredSelector);
			searchInTheWholeHierarchy.
	self 
		assertCollection: senders 
		hasSameElements: {
			SycMockClassForHierarhicalSenderA >> #methodReferToSelector . 
			SycMockClassForHierarhicalSenderC >> #methodReferToSelector. 
			SycMockClassForHierarhicalSenderB >> #targetMethod }.
]
