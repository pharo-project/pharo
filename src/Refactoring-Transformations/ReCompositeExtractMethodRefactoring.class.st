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
	#name : 'ReCompositeExtractMethodRefactoring',
	#superclass : 'RBCompositeMethodTransformation',
	#instVars : [
		'sourceCode',
		'newSelector',
		'newMethod',
		'parseTree',
		'subtree',
		'arguments',
		'temporaries',
		'assignments',
		'needsReturn'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'utilities' }
ReCompositeExtractMethodRefactoring class >> allMethodsInHierarchyOf: aRBClass [

	^ ((aRBClass
		withAllSuperclassesUntil: Object)
		flatCollect: [ :class | class allMethods collect: [:each | each method] ])
		reject: #isNil
]

{ #category : 'api' }
ReCompositeExtractMethodRefactoring class >> extract: aString from: aSelector to: aNewSelector in: aClassName [

	^ self new
		extract: aString
		from: aSelector
		to: aNewSelector
		in: aClassName;
		yourself
]

{ #category : 'api' }
ReCompositeExtractMethodRefactoring class >> model: aRBModel extract: aString from: aSelector to: aNewSelector in: aClassName [

	^ self new
		model: aRBModel;
		extract: aString
		from: aSelector
		to: aNewSelector
		in: aClassName;
		yourself
]

{ #category : 'execution' }
ReCompositeExtractMethodRefactoring >> addReturnIfNeeded: newMethodNode [

	(subtree parent isUsingAsReturnValue: subtree) ifTrue: [
		newMethodNode addReturn ].
	
	assignments size = 1
		ifTrue: [
			newMethodNode addNode: (RBReturnNode value:
					 (RBVariableNode named: assignments first asString)) ]
		ifFalse: [ RBReturnNodeAdderVisitor new visit: newMethodNode ].
]

{ #category : 'preconditions' }
ReCompositeExtractMethodRefactoring >> applicabilityPreconditions [

	^ self preconditionParsableSubtree
	  & self preconditionNotInCascadedMessage
	  & self preconditionTemporariesAreNotReadBeforeWritten
	  & self preconditionOneAssignmentMaximum
	  & self preconditionAssignmentsNotReadBeforeWritten
	  & self preconditionSubtreeDoesNotContainsReturn
]

{ #category : 'accessing' }
ReCompositeExtractMethodRefactoring >> arguments [
	^ arguments
]

{ #category : 'preconditions' }
ReCompositeExtractMethodRefactoring >> breakingChangePreconditions [ 

	  ^ self preconditionHasSameExitPoint 
]

{ #category : 'executing' }
ReCompositeExtractMethodRefactoring >> buildTransformationFor: newMethodName [

	| messageSend |
	messageSend := self messageSendWith: newMethodName.

	^ OrderedCollection new
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
		  add: (ReRemoveUnusedTemporaryVariableRefactoring
				   model: self model
				   inMethod: selector
				   inClass: class name);
		  yourself
]

{ #category : 'executing' }
ReCompositeExtractMethodRefactoring >> buildTransformations [

	| newMethodName existingMethod checker messageSend |
	checker := EquivalentTreeChecker new
					model: model;
					on: class;
					extractedFromSelector: selector.
					
	newMethodName := self newMethodName.
	newMethod := self generateNewMethodWith: newMethodName.
	existingMethod := checker findEquivalentTreeFor: newMethod.
	existingMethod ifNil: [ ^ self buildTransformationFor: newMethodName ].
	
	messageSend := self messageSendWith: existingMethod ast.
	^ OrderedCollection with: 
				(RBReplaceSubtreeTransformation
				   model: self model
				   replace: sourceCode
				   to: messageSend
				   inMethod: selector
				   inClass: class)
]

{ #category : 'querying' }
ReCompositeExtractMethodRefactoring >> calculateArguments [
	"returns a collection of variables that should be passed as arguments"

	| allVariables accesses assigned |
	allVariables := self sourceMethodTree allDefinedVariables difference: subtree allDefinedVariables.

	accesses := allVariables select: [ :each | subtree references: each ].
	assigned := allVariables select: [ :each | subtree assigns: each ].

	^ accesses asOrderedCollection difference: assigned
]

{ #category : 'querying' }
ReCompositeExtractMethodRefactoring >> calculateAssignments [
	"checks how many variables in the subtree are assigned to values."
	"Those are temporaries (without temps defined in the subtree; see `calculateTemporaries` for details)
	that have reference in a statement outside of the subtree.
	For those variables we would need to return newly assigned value to preserve behavior."
	
	^ assignments ifNil: [
		| sequenceNode whoDefinesTemp |
		sequenceNode := (self sourceMethodTree allChildren select: [ :each | each isSequence ]) last.

		assignments := temporaries select: [ :temp |
			whoDefinesTemp := (sequenceNode whoDefines: temp).
			whoDefinesTemp statements anySatisfy: [ :statement |
				(statement references: temp)
				and: [ (subtree allChildren includes: statement) not ] ] ] ]
]

{ #category : 'querying' }
ReCompositeExtractMethodRefactoring >> calculateIfReturnIsNeededInCaller [

	| searcher |
	searcher := self parseTreeSearcher.
	searcher
		matches: '^self' do: [:aNode :answer | answer];
		matches: '^`@anything' do: [:aNode :answer | true].
	^ (searcher executeTree: subtree initialAnswer: false)
]

{ #category : 'querying' }
ReCompositeExtractMethodRefactoring >> calculateSubtree [

	^ subtree ifNil: [
		subtree := self sourceMethodTree ifNotNil: [ :t | t extractSubtreeWith: sourceCode ] ]
]

{ #category : 'querying' }
ReCompositeExtractMethodRefactoring >> calculateTemporaries [
	"returns a collection of variables that should be defined inside the extracted method.
	Those are all variables (temps and args) that are defined outside of subtree,
	but are part of an assignment in the subtree.
	If we want to assign something to them, we need to have a temp for it.
	See calculateAssignments which then checks if we can preserve behavior
	by returning that assignment value as extracted method's return value"
	
	
	^ temporaries ifNil: [
		| allVariables accesses |
		allVariables := self sourceMethodTree allDefinedVariables difference: subtree allDefinedVariables.
		accesses := allVariables select: [ :each | subtree references: each ].

		temporaries := accesses select: [ :each | subtree assigns: each ] ]
]

{ #category : 'scripting api - conditions' }
ReCompositeExtractMethodRefactoring >> checkPreconditions [

	super checkPreconditions.
	self breakingChangePreconditions check ifFalse: [
		self refactoringWarning:
			self breakingChangePreconditions errorString ]
]

{ #category : 'instance creation' }
ReCompositeExtractMethodRefactoring >> extract: aString from: aSelector in: aClassName [ 
	class := self model classNamed: aClassName.
	selector := aSelector.
	sourceCode := aString
]

{ #category : 'api' }
ReCompositeExtractMethodRefactoring >> extract: aString from: aSelector to: aNewSelector in: aClassName [
	"partial instantiation."
	
	self extract: aString from: aSelector in: aClassName.
	newSelector := aNewSelector.
	
]

{ #category : 'executing' }
ReCompositeExtractMethodRefactoring >> generateNewMethodWith: aMethodName [

	| args newMethodNode |
	args := aMethodName arguments collect: [ :p |
		        RBVariableNode named: p ].

	newMethodNode := RBMethodNode
		                 selector: newSelector
		                 arguments: args asArray
		                 body: (subtree isSequence
				                  ifTrue: [ subtree ]
				                  ifFalse: [
					                  RBSequenceNode
						                  temporaries: #(  )
						                  statements: (OrderedCollection with: subtree) ]).
	temporaries do: [ :each | newMethodNode body addTemporaryNamed: each ].
	
	self addReturnIfNeeded: newMethodNode.

	^ newMethodNode
]

{ #category : 'utilities' }
ReCompositeExtractMethodRefactoring >> mapArguments: args toValues: values in: aTree [
	self flag: #toRemove.
	^ (args size = values size)
		"map directly"
		ifTrue: [
			| argsAndValues |
			argsAndValues := OrderedCollection new.
			args with: values do: [ :arg :val |
				argsAndValues add: (arg name asString -> val value storeString) ].
			argsAndValues ]
		"open browser"
		ifFalse: [
			(RBMethodArgumentsSelector
				openOn: (args collect: [:e | e name asString])
				and: (values collect: [ :e | e value storeString ])
				in: aTree)
				argumentsAndValues ]
]

{ #category : 'executing' }
ReCompositeExtractMethodRefactoring >> messageSendWith: aMethodName [

	^ String streamContents: [ :string |
		needsReturn ifTrue: [ string nextPutAll: '^ ' ].

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
									string nextPutAll: arg asString.
								string nextPut: $ ] ] ] ]
]

{ #category : 'executing' }
ReCompositeExtractMethodRefactoring >> newMethodName [

	| methodName newAttempt |
	newAttempt := newSelector isNil.

	methodName := RBMethodName new.
	methodName arguments: arguments.
	newSelector ifNotNil: [ methodName selector: newSelector ].

	[ newAttempt ] whileTrue: [
		methodName := (RBMethodNameEditor openOn: methodName) methodName.
		methodName
			ifNil: [ newAttempt := false ]
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

{ #category : 'accessing' }
ReCompositeExtractMethodRefactoring >> newSelector: aSymbol [

	newSelector := aSymbol
]

{ #category : 'preconditions' }
ReCompositeExtractMethodRefactoring >> preconditionAssignmentsNotReadBeforeWritten [

	assignments isEmptyOrNil ifTrue: [ ^ self trueCondition ].

	^ ReVariablesNotReadBeforeWrittenCondition new subtree: subtree; variables: assignments
]

{ #category : 'preconditions' }
ReCompositeExtractMethodRefactoring >> preconditionHasSameExitPoint [

	^ RBCondition
		withBlock: [ subtree hasSameExitPoint ]
		errorString: 'You cannot extract guard clauses and other expressions that directly impact the methods execution flow.'
]

{ #category : 'preconditions' }
ReCompositeExtractMethodRefactoring >> preconditionNotInCascadedMessage [

	^ ReNotInCascadedMessageCondition new subtree: subtree
]

{ #category : 'preconditions' }
ReCompositeExtractMethodRefactoring >> preconditionOneAssignmentMaximum [
	"When we have one assignment to temp or arg we don't need to worry about other references,
	since we can return newly assigned value from the extracted method and preserve behavior.
	When we have two or more assignments AND when they have references outside of extracted block,
	we don't support return of multiple values, instead we notify the user that all references
	to those temps should be extracted as well."

	^ ReMaxOneAssignmentWithReferencesCondition new
		  assignments: assignments
]

{ #category : 'preconditions' }
ReCompositeExtractMethodRefactoring >> preconditionParsableSubtree [

	^ RBCondition
		  withBlock: [ self sourceMethodTree isNotNil & subtree isNotNil ]
		  errorString: 'Cannot extract selected code, it is not a valid subtree.'
]

{ #category : 'preconditions' }
ReCompositeExtractMethodRefactoring >> preconditionSubtreeDoesNotContainsReturn [

	assignments isEmptyOrNil ifTrue: [ ^ self trueCondition ].

	^ ReSubtreeDoesNotContainReturnCondition new subtree: subtree
]

{ #category : 'preconditions' }
ReCompositeExtractMethodRefactoring >> preconditionTemporariesAreNotReadBeforeWritten [

	^ ReVariablesNotReadBeforeWrittenCondition new subtree: subtree; variables: temporaries 
]

{ #category : 'transforming' }
ReCompositeExtractMethodRefactoring >> prepareForExecution [

	subtree := self calculateSubtree.
	subtree ifNotNil: [  
		temporaries := self calculateTemporaries.
		assignments := self calculateAssignments.
		arguments := self calculateArguments.
		needsReturn := self calculateIfReturnIsNeededInCaller]
]

{ #category : 'utilities' }
ReCompositeExtractMethodRefactoring >> selectorStartingFrom: aString argumentsSize: aNumber [

	| str | 
	str := aString.
	aNumber timesRepeat: [ str := str, '_:' ].
	^ str asSymbol		
]

{ #category : 'querying' }
ReCompositeExtractMethodRefactoring >> sourceMethodTree [

	^ parseTree ifNil: [ parseTree := self definingMethod ]
]

{ #category : 'storing' }
ReCompositeExtractMethodRefactoring >> storeOn: aStream [

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
