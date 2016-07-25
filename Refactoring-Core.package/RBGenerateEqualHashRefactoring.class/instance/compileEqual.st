transforming
compileEqual
	| method statement comparison |
	method := RBParser parseMethod: '= anObject
		"Answer whether the receiver and anObject represent the same object."

		self == anObject ifTrue: [ ^ true ].
		self class = anObject class ifFalse: [ ^ false ]'.
	statement := nil.	
	variables reversed do: [ :each |
		| accessor |
		accessor := self accessorForVariable: each.
		comparison := RBMessageNode 
			receiver: (RBVariableNode named: each) 
			selector: #=
			arguments: (Array with: (RBMessageNode 
				receiver: (RBVariableNode named: 'anObject')
				selector: accessor)).
		statement := statement isNil
			ifTrue: [ comparison ]
			ifFalse: [
				RBMessageNode
					receiver: comparison
					selector: #and:
					arguments: (Array with: (RBBlockNode 
						body: (RBSequenceNode 
							statements: (Array with: statement)))) ] ].
	method addNode: statement; addReturn.
	self theClass 
		compile: method formattedCode
		classified: #(comparing)