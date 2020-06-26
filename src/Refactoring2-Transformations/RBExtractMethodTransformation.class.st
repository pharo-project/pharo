"
Creates a method from a code fragment. Temporary variables and arguments are automatically calculated by the transformation.

If the name of the new method is not provided (i.e., nil), it prompts a dialog window so the developer provides a valid method name. In this new implementation, it is possible to add or remove arguments in the new extracted message.
Similarly, if the number of arguments in the new method provided by the developer is higher than the number of arguments as calculated by the transformation, it prompts a dialog window so the developer selects which values must be passed as arguments.

Usage:
transformation := (RBExtractMethodTransformation
	extract: '(RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false)
		ifFalse: [builder
					compile: rewriteRule tree printString
					in: class
					classified: aSmalllintContext protocols]'
	from: #checkMethod:
	to: #foo:
	in: #RBTransformationRuleTest)
	transform.
(ChangesBrowser changes: transformation model changes changes) open
"
Class {
	#name : #RBExtractMethodTransformation,
	#superclass : #RBCompositeMethodTransformation,
	#instVars : [
		'sourceCode',
		'newSelector',
		'newMethod',
		'parseTree',
		'subtree',
		'arguments',
		'temporaries',
		'assignments'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #utilities }
RBExtractMethodTransformation class >> allMethodsInHierarchyOf: aRBClass [

	^ ((aRBClass
		withAllSuperclassesUntil: Object)
		flatCollect: [ :class | class allMethods collect: [:each | each method] ])
		reject: #isNil
]

{ #category : #api }
RBExtractMethodTransformation class >> extract: aString from: aSelector to: aNewSelector in: aClass [

	^ self new
		extract: aString
		from: aSelector
		to: aNewSelector
		in: aClass;
		yourself
]

{ #category : #api }
RBExtractMethodTransformation class >> model: aRBModel extract: aString from: aSelector to: aNewSelector in: aClass [

	^ self new
		model: aRBModel;
		extract: aString
		from: aSelector
		to: aNewSelector
		in: aClass;
		yourself
]

{ #category : #executing }
RBExtractMethodTransformation >> buildTransformations [

	subtree := self calculateSubtree.
			
	^ (parseTree isNil or: [ subtree isNil ])
		ifTrue: [ OrderedCollection new ]
		ifFalse: [ 
			| tempsToRemove |
			arguments := self calculateArguments.
			temporaries := self calculateTemporaries.
			assignments := self calculateAssignments.
			tempsToRemove := self calculateTemporariesToRemove.
			
			assignments size > 1
			ifTrue: [ OrderedCollection new ]
			ifFalse: [ 
				| newMethodName newArguments messageSend needsReturn |
				newMethodName := self newMethodName.
				newMethodName ifNil: [ ^ OrderedCollection new ].
				
				needsReturn := self calculateIfReturnIsNeeded.
				newMethod := self generateNewMethodWith: newMethodName.
				newArguments := self calculateNewArgumentsIn: newMethodName.
				messageSend := self messageSendWith: newMethodName 
									and: newArguments needsReturn: needsReturn.
										
				OrderedCollection new
					add: (RBAddMethodTransformation
							model: self model
							sourceCode: newMethod newSource
							in: class
							withProtocol: Protocol unclassified);
					add: (RBReplaceSubtreeTransformation
							model: self model
							replace: sourceCode
							to: messageSend
							inMethod: selector
							inClass: class);
					addAll: (tempsToRemove collect: [ :temporary |
							RBRemoveTemporaryVariableTransformation
							model: self model
							variable: temporary
							inMethod: selector
							inClass: class ]);
					yourself ] ]
]

{ #category : #querying }
RBExtractMethodTransformation >> calculateArguments [
	"returns a collection of variable that should be passed as arguments"

	| allVariables accesses assigned |
	allVariables := parseTree allDefinedVariables difference: subtree allDefinedVariables.
	
	accesses := allVariables select: [ :each | subtree references: each ].
	assigned := allVariables select: [ :each | subtree assigns: each ].
	
	^ accesses asOrderedCollection difference: assigned.
	
]

{ #category : #querying }
RBExtractMethodTransformation >> calculateAssignments [
	"checks how many variables in the subtree are assigned to values."

	^ assignments ifNil: [ 
		| sequenceNode whoDefinesTemp |
		sequenceNode := (parseTree allChildren select: #isSequence) last.
	
		assignments := temporaries select: [ :temp | 
			whoDefinesTemp := (sequenceNode whoDefines: temp).
			whoDefinesTemp statements anySatisfy: [ :statement |
				(statement references: temp)
				and: [ (subtree allChildren includes: statement) not ] ] ] ]
]

{ #category : #querying }
RBExtractMethodTransformation >> calculateIfReturnIsNeeded [

	| searcher |
	searcher := self parseTreeSearcher.
	searcher
		matches: '^self' do: [:aNode :answer | answer];
		matches: '^`@anything' do: [:aNode :answer | true].
	^ (searcher executeTree: subtree initialAnswer: false)
]

{ #category : #querying }
RBExtractMethodTransformation >> calculateNewArgumentsIn: aMethodName [

	aMethodName arguments size > arguments size ifTrue: [ 
		| selection |
		selection := RBMethodArgumentsSelector
			openOn: aMethodName arguments copy
			and: (newMethod body allChildren
					select: #isLiteralNode
					thenCollect: [ :e | e value storeString ])
			in: newMethod.
			
		newMethod := selection newTree.
		^ selection argumentsAndValues ].
	^ Dictionary new
]

{ #category : #querying }
RBExtractMethodTransformation >> calculateSubtree [

	^ subtree ifNil: [ 
		subtree := self calculateTree ifNotNil: [ parseTree extractSubtreeWith: sourceCode ] ]
]

{ #category : #querying }
RBExtractMethodTransformation >> calculateTemporaries [
	"returns a collection of variables that should be defined inside the method"

	^ temporaries ifNil: [
		| allVariables accesses |
		allVariables := parseTree allDefinedVariables difference: subtree allDefinedVariables.
		accesses := allVariables select: [ :each | subtree references: each ].
	
		temporaries := accesses select: [ :each | subtree assigns: each ] ]
]

{ #category : #querying }
RBExtractMethodTransformation >> calculateTemporariesToRemove [
	"returns a collection of variables that should be removed from the original method"

	| newParseTree |
	newParseTree := parseTree copy removeSubtree: subtree.
	^ newParseTree allDefinedVariables
		reject: [ :var | newParseTree references: var ]
]

{ #category : #querying }
RBExtractMethodTransformation >> calculateTree [

	^ parseTree ifNil: [ parseTree := self definingMethod ]
]

{ #category : #api }
RBExtractMethodTransformation >> extract: aString from: aSelector to: aNewSelector in: aClassName [

	class := aClassName.
	selector := aSelector.
	newSelector := aNewSelector.
	sourceCode := aString
]

{ #category : #executing }
RBExtractMethodTransformation >> generateNewMethodWith: aMethodName [

	| args newMethodNode |
	args := aMethodName arguments
		collect: [ :p | RBVariableNode named: p ].
	
	newMethodNode := RBMethodNode
							selector: aMethodName selector
							arguments: args asArray
							body: (subtree isSequence 
								ifTrue: [ subtree ]
								ifFalse: [ RBSequenceNode
									temporaries: #()
									statements: (OrderedCollection with: subtree)]).						
	temporaries do: [:each | newMethodNode body addTemporaryNamed: each].
	
	((parseTree parentOfSubtree: subtree) uses: subtree)
		ifTrue: [ newMethodNode addReturn ].
	
	assignments size = 1 ifTrue: [
		newMethodNode addNode: 
			(RBReturnNode value: 
			(RBVariableNode named: assignments first asString)) ].
	
	^ newMethodNode.
]

{ #category : #executing }
RBExtractMethodTransformation >> messageSendWith: aMethodName and: newArguments needsReturn: aBoolean [

	^ String streamContents: [ :string |
		aBoolean ifTrue: [ string nextPutAll: '^ ' ].
			
		assignments size = 1
			ifTrue: [ string
				nextPutAll: assignments first asString;
				nextPutAll: ' := ' ].
		
		string nextPutAll: 'self '.
		aMethodName arguments
			ifEmpty: [ string nextPutAll: aMethodName selector asString ] 
			ifNotEmpty: [ 
				(aMethodName selector keywords size = aMethodName arguments size)
					ifTrue: [ aMethodName selector keywords
								with: aMethodName arguments 
								do: [ :key :arg | 
									string nextPutAll: key asString; nextPut: $ .
									(newArguments includesKey: arg asString)
										ifTrue: [ string nextPutAll: (newArguments at: arg asString) ]
										ifFalse: [ string nextPutAll: arg asString ].
								string nextPut: $ ] ] ] ]
]

{ #category : #executing }
RBExtractMethodTransformation >> newMethodName [

	| methodName newAttempt |
	newAttempt := newSelector isNil.
	
	methodName := RBMethodName new.
	methodName arguments: arguments.
	newSelector ifNotNil: [ methodName selector: newSelector ].
	
	[ newAttempt ] whileTrue: [ 
		methodName := (RBMethodNameEditor openOn: methodName) methodName.
		methodName
			ifNil: [ "self inform: 'Please provide a name for the method.'.
						methodName := RBMethodName new arguments: arguments" newAttempt := false ]
			ifNotNil: [ :newMethodName |
				newSelector := newMethodName selector.
				newAttempt := newSelector isNil.
				
				"it's a valid selector"
				(newSelector isString and: [newSelector isValidSelector])
					ifFalse: [ self inform: newSelector asString, ' is not a valid selector.'.
								  newSelector := nil ].
					
				"already exists in class"
				(self definingClass directlyDefinesLocalMethod: newSelector)
					ifTrue: [ self inform: newSelector, ' is already defined in ', class asString.
								 newSelector := nil ] ] ].

	^ methodName
]

{ #category : #preconditions }
RBExtractMethodTransformation >> preconditions [

	| aSubtree |
	aSubtree := self calculateSubtree.
	
	^ (RBCondition withBlock: [ 
		aSubtree ifNil: [ self refactoringError: 'Cannot extract code.' ]. true ])
		"subtree containsReturn
		ifTrue: [ self refactoringError: 'Cannot extract code because it contains a return.' ]."
		& (RBCondition withBlock: [
			aSubtree parent isCascade ifTrue: [ 
				self refactoringError: 'Cannot extract code in a cascaded message' ]. true])
		& (RBCondition withBlock: [
			| tempVariables |
			tempVariables := self calculateTemporaries.
			(RBReadBeforeWrittenTester readBeforeWritten: tempVariables in: aSubtree) ifNotEmpty: [
				self refactoringError: 'Cannot extract temporaries if they are read before written.' ]. true ])
		& (RBCondition withBlock: [
			self calculateAssignments size > 1 ifTrue: [ 
				self refactoringError: 'Cannot extract two or more assignments to temporaries without also extracting all the references.' ]. true ])
		& (RBCondition withBlock: [
			| assignmentCollection |
			assignmentCollection := self calculateAssignments.
			assignmentCollection ifNotEmpty: [ 
			((RBReadBeforeWrittenTester isVariable: assignmentCollection first
				readBeforeWrittenIn: aSubtree) 
				or: [aSubtree containsReturn])
				ifTrue: [ self refactoringError: ('Cannot extract assignment to <1s> without also extracting all the references.' expandMacrosWith: assignments asString)] ]. true ])
		& (RBCondition withBlock: [	
			| searchSpace |
			searchSpace := (self class
				allMethodsInHierarchyOf: self definingClass)
				reject: [ :m | m selector = selector ].
			
			(self parseTreeSearcherClass whichMethodIn: searchSpace matches: aSubtree)
				ifNotEmpty: [ :opportunities |
					self refactoringError: ('<1s> method(s) already implement this code.<n>Do you want to send a message instead?' expandMacrosWith: opportunities size asString)
					with: [ (self parseTreeRewriterClass 
							  replaceCode: aSubtree
							  byMessageSendTo: opportunities anyOne
							  in: (self definingClass methodFor: self calculateTree selector))
							  transform ]. false ]
				ifEmpty: [ true ] ])
]

{ #category : #printing }
RBExtractMethodTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' extract: '''.
	sourceCode storeOn: aStream.
	aStream
		nextPutAll: ''' from: #';
		nextPutAll: selector asString;
		nextPutAll: ' to: #';
		nextPutAll: newSelector asString;
		nextPutAll: ' in: '.
	class storeOn: aStream.
	aStream nextPut: $)
]
