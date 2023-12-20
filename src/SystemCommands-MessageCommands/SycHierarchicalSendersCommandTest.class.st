Class {
	#name : 'SycHierarchicalSendersCommandTest',
	#superclass : 'TestCase',
	#category : 'SystemCommands-MessageCommands-Tests',
	#package : 'SystemCommands-MessageCommands',
	#tag : 'Tests'
}

{ #category : 'tests' }
SycHierarchicalSendersCommandTest >> testSearchInTheWholeHierarchy [

	| senders |
	senders := SycHierarchicalSendersCommand new
			refactoredClass: SycMockClassForHierarhicalSenderB;
			originalMessage: (SycMockClassForHierarhicalSenderB >> #referredSelector);
			searchInTheWholeHierarchy.
	self 
		assertCollection: senders 
		hasSameElements: {
			SycMockClassForHierarhicalSenderA >> #methodReferToSelector . 
			SycMockClassForHierarhicalSenderC >> #methodReferToSelector. 
			SycMockClassForHierarhicalSenderB >> #targetMethod }.
]
