Extension { #name : #RBProgramNode }

{ #category : #'*opalcompiler-core' }
RBProgramNode >> doSemanticAnalysis [
	self methodNode ifNil: [ ^self ].
	^ self methodNode doSemanticAnalysis.
	
]

{ #category : #'*opalcompiler-core' }
RBProgramNode >> enclosingMethodOrBlockNode [
	^ self methodOrBlockNode 
]

{ #category : #'*opalcompiler-core' }
RBProgramNode >> irInstruction [
	^ self methodOrBlockNode ir firstInstructionMatching: [:instr | instr sourceNode == self ]
]

{ #category : #'*opalcompiler-core' }
RBProgramNode >> isClean [
	^ self children allSatisfy: [ :child | child isClean ]
]

{ #category : #'*opalcompiler-core' }
RBProgramNode >> methodOrBlockNode [
	^ parent ifNotNil: [ parent methodOrBlockNode ]
]

{ #category : #'*opalcompiler-core' }
RBProgramNode >> owningScope [

	^ parent owningScope 
]

{ #category : #'*opalcompiler-core' }
RBProgramNode >> printAsIfCompiledOn: aStream [ 
	aStream nextPutAll: self formattedCode
]

{ #category : #'*opalcompiler-core' }
RBProgramNode >> scope [
	^ self methodOrBlockNode scope
]
