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
		messages: { SycMessageDescription ofMethod: (SycMockClassForHierarchicalSenderB >> #referredSelector) };
		searchInTheWholeHierarchy.
	self 
		assertCollection: senders 
		hasSameElements: {
			SycMockClassForHierarchicalSenderA >> #methodReferToSelector . 
			SycMockClassForHierarchicalSenderC >> #methodReferToSelector. 
			SycMockClassForHierarchicalSenderB >> #targetMethod }.
]
