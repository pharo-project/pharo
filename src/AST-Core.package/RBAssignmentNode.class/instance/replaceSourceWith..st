replacing
replaceSourceWith: aNode 
	"Check if we need to convert the assignment. Also check if we are being replaced with a setter message send. If so, create the replacements to edit the original source."

	(aNode isAssignment and: [ aNode assignmentOperator ~= self assignmentOperator ]) ifTrue: [
		self addReplacement: (RBStringReplacement 
			replaceFrom: self assignmentPosition
			to: self assignmentPosition + self assignmentOperator size - 1
			with: aNode assignmentOperator).
		(aNode variable = variable and: [ aNode value = value ])
			ifTrue: [ ^ self ] ].
	aNode isMessage ifFalse: [^super replaceSourceWith: aNode].
	aNode receiver isVariable ifFalse: [^super replaceSourceWith: aNode].
	aNode numArgs = 1 ifFalse: [^super replaceSourceWith: aNode].
	(self mappingFor: self value) = aNode arguments first 
		ifFalse: [^super replaceSourceWith: aNode].
	(self value hasParentheses not 
		and: [aNode arguments first precedence >= aNode precedence]) 
			ifTrue: 
				[self
					addReplacement: (RBStringReplacement 
								replaceFrom: self value start
								to: self value start - 1
								with: '(');
					addReplacement: (RBStringReplacement 
								replaceFrom: self value stop + 1
								to: self value stop
								with: ')')].
	self addReplacement: (RBStringReplacement 
				replaceFrom: self variable start
				to: self assignmentPosition + 1
				with: aNode receiver name , ' ' , aNode selector)