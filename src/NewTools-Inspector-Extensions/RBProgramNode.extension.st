Extension { #name : #RBProgramNode }

{ #category : #'*NewTools-Inspector-Extensions' }
RBProgramNode >> inspectionASTDump [
	<inspectorPresentationOrder: 50 title: 'AST Dump'>

	^ SpCodePresenter new 
		text: (RBParser parseExpression: self dump) formattedCode; 
		yourself
]

{ #category : #'*NewTools-Inspector-Extensions' }
RBProgramNode >> inspectionSourceCode [
	<inspectorPresentationOrder: 30 title: 'Source code'>

	^ SpCodePresenter new 
		beForBehavior: (self withAllParents  first compilationContext ifNotNil: [ :ctx | ctx getClass ]);
		text: self source;
		selectionInterval: self sourceInterval;
		yourself
]

{ #category : #'*NewTools-Inspector-Extensions' }
RBProgramNode >> inspectionTree [
	<inspectorPresentationOrder: 35 title: 'Tree'>

	^ SpTreePresenter new 
		roots: { self };
		children: [ :aNode | aNode children ];
		display: [ :each | 
			String
				streamContents: [ :stream | 
					stream
						nextPutAll: each class name;
						nextPut: $(;
						nextPutAll: ((each formattedCode truncateTo: 50) copyReplaceAll: String cr with: String space);
						nextPut: $)
			 ] ];
		yourself
]
