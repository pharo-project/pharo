"
A SpRecursiveContextSetterTest is a test class for testing the behavior of SpRecursiveContextSetter
"
Class {
	#name : #SpRecursiveContextSetterTest,
	#superclass : #TestCase,
	#category : #'Spec2-Commander2-Tests'
}

{ #category : #test }
SpRecursiveContextSetterTest >> testVisitCommand [
	| commandsGroup |
	commandsGroup := (CmCommandGroup named: 'root') asSpecGroup beRoot
		register:
			(CmBlockCommand new
				name: 'c1';
				block: [  ];
				context: 42;
				yourself) asSpecCommand;
		register:
			((CmCommandGroup named: 'sub1') asSpecGroup
				register:
					(CmBlockCommand new
						name: 'c2';
						block: [  ];
						context: 42;
						yourself) asSpecCommand;
				register:
					(CmBlockCommand new
						name: 'c3';
						block: [  ];
						yourself) asSpecCommand);
		register:
			(CmBlockCommand new
				name: 'c4';
				block: [  ];
				yourself) asSpecCommand;
		yourself.
	self assert: (commandsGroup / 'c1') context equals: 42.
	self assert: (commandsGroup / 'sub1' / 'c2') context equals: 42.
	self assert: (commandsGroup / 'sub1' / 'c3') context isNil.
	self assert: (commandsGroup / 'c4') context isNil.
	SpRecursiveContextSetter visit: commandsGroup toSetContext: 43.
	self assert: (commandsGroup / 'c1') context equals: 42.
	self assert: (commandsGroup / 'sub1' / 'c2') context equals: 42.
	self assert: (commandsGroup / 'sub1' / 'c3') context equals: 43.
	self assert: (commandsGroup / 'c4') context equals: 43
]
