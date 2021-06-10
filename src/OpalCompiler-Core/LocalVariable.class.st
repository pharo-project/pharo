"
I model local Variables
"
Class {
	#name : #LocalVariable,
	#superclass : #Variable,
	#instVars : [
		'escaping',
		'index',
		'scope',
		'usage'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #testing }
LocalVariable class >> isAbstract [ 
	^ self = LocalVariable  
]

{ #category : #comparing }
LocalVariable >> = aTempVar [

	^aTempVar class = self class 
		and: [aTempVar scope = self scope 
		and: [aTempVar name = self name
		and: [aTempVar usage = self usage]]].

	
]

{ #category : #visiting }
LocalVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitLocalVariableNode: aNode
]

{ #category : #'read/write usage' }
LocalVariable >> analyzeRead: aVariableNode by: aSemanticAnalyzer [
	super analyzeRead: aVariableNode by: aSemanticAnalyzer.

	aSemanticAnalyzer analyzeLocalVariableRead: self
]

{ #category : #'read/write usage' }
LocalVariable >> analyzeWrite: aVariableNode by: aSemanticAnalyzer [
	super analyzeWrite: aVariableNode by: aSemanticAnalyzer.

	aSemanticAnalyzer analyzeLocalVariableWrite: self
]

{ #category : #converting }
LocalVariable >> asDoItVariableFrom: aContext [
	^ DoItVariable fromContext: aContext variable: self
]

{ #category : #converting }
LocalVariable >> asString [

	^ self name
]

{ #category : #queries }
LocalVariable >> astNodes [
	^scope node methodNode variableNodes select: [ :each | each variable == self]
]

{ #category : #converting }
LocalVariable >> createCopiedVariableFor: aScope [
	^ CopiedLocalVariable new
			originalVar: self originalVar;
			name: self name;
			escaping: self escaping;
			usage: self usage;
			scope: aScope
]

{ #category : #emitting }
LocalVariable >> emitStore: methodBuilder [

	methodBuilder storeTemp: name. 
]

{ #category : #emitting }
LocalVariable >> emitValue: methodBuilder [

	methodBuilder pushTemp: name.
]

{ #category : #escaping }
LocalVariable >> escaping [
	^escaping
]

{ #category : #escaping }
LocalVariable >> escaping: anObject [
	escaping := anObject
]

{ #category : #comparing }
LocalVariable >> hash [

	^ name hash bitXor: (usage hash bitXor: scope hash).
]

{ #category : #accessing }
LocalVariable >> index [
	^ index ifNil: [ 
		"if the index is nil, we are in an AST after name analysis but before
		IR generation. To fill the the index, we generate the ir."
		scope node methodNode ir. 
		index ifNil: [ self error: 'no temp index, should never happen' ] ]
]

{ #category : #accessing }
LocalVariable >> index: anObject [
	index := anObject
]

{ #category : #initialization }
LocalVariable >> initialize [
	super initialize.
	escaping := false.
]

{ #category : #testing }
LocalVariable >> isAccessedIn: aMethod [

	^ self isUsed and: [ aMethod == self method ]
]

{ #category : #testing }
LocalVariable >> isCopying [
	^false
]

{ #category : #testing }
LocalVariable >> isDefinedByBlock [
	"true if a variable node is defined by a block"
	^scope isBlockScope
]

{ #category : #escaping }
LocalVariable >> isEscaping [
	^escaping = #escapingRead or: [escaping = #escapingWrite]
]

{ #category : #escaping }
LocalVariable >> isEscapingRead [
	^escaping = #escapingRead

]

{ #category : #escaping }
LocalVariable >> isEscapingWrite [
	^escaping = #escapingWrite

]

{ #category : #testing }
LocalVariable >> isLocalVariable [

	^true
]

{ #category : #'read/write usage' }
LocalVariable >> isRead [
	^usage = #read

]

{ #category : #testing }
LocalVariable >> isReferenced [
	^ self isUsed
]

{ #category : #testing }
LocalVariable >> isRemote [
	^false
]

{ #category : #'read/write usage' }
LocalVariable >> isRepeatedWrite [
	^usage = #repeatedWrite

]

{ #category : #testing }
LocalVariable >> isStoringTempVector [
	"I am a temp that stores a temp vector. Those generated temps have a invalid name starting with 0"
	^name first = $0.
]

{ #category : #testing }
LocalVariable >> isTempVectorTemp [
	^false
]

{ #category : #'read/write usage' }
LocalVariable >> isUninitialized [

	^ self isWrite not
]

{ #category : #testing }
LocalVariable >> isUsed [
	"when the var is never read or written, it is not used"
	^ usage isNotNil
]

{ #category : #'read/write usage' }
LocalVariable >> isWrite [
	^ usage = #write or: [ self isRepeatedWrite ]
]

{ #category : #escaping }
LocalVariable >> markEscapingRead [
	escaping = #escapingWrite ifFalse: [escaping := #escapingRead]
]

{ #category : #escaping }
LocalVariable >> markEscapingWrite [
	escaping := #escapingWrite.
	self isRepeatedWrite ifFalse: [usage := #write]
]

{ #category : #'read/write usage' }
LocalVariable >> markRead [
	"reading does not change a #write, nor an #arg"
	usage ifNil: [usage := #read]
]

{ #category : #escaping }
LocalVariable >> markRepeatedWrite [
	"same as write"
	self markWrite.
	usage := #repeatedWrite
]

{ #category : #escaping }
LocalVariable >> markWrite [

	"if an escaping var is wrote to later, it needs to be remote"
	self isEscaping 
		ifTrue: [self markEscapingWrite].
	"write is the strongest use: a read is turned into a write"
	usage := #write.
]

{ #category : #queries }
LocalVariable >> method [
	^scope node methodNode compiledMethod
]

{ #category : #accessing }
LocalVariable >> originalVar [
	^ self
]

{ #category : #printing }
LocalVariable >> printOn: stream [

	stream nextPutAll: self name
]

{ #category : #debugging }
LocalVariable >> readFromContext: aContext scope: contextScope [
	| definitionContext |
	definitionContext := contextScope lookupDefiningContextForVariable: self startingFrom: aContext.
	^ self readFromLocalContext: definitionContext
]

{ #category : #debugging }
LocalVariable >> readFromLocalContext: aContext [

	^ aContext tempAt: self index
]

{ #category : #debugging }
LocalVariable >> readInContext: aContext [

	| contextScope |
	contextScope := aContext astScope.
	^self readFromContext: aContext scope: contextScope
]

{ #category : #accessing }
LocalVariable >> scope [
	"this is the scope that the variable is declared in"
	^ scope
]

{ #category : #initialization }
LocalVariable >> scope: aLexicalScope [

	scope := aLexicalScope
]

{ #category : #accessing }
LocalVariable >> usage [

	^ usage
]

{ #category : #accessing }
LocalVariable >> usage: anObject [

	usage := anObject
]

{ #category : #queries }
LocalVariable >> usingMethods [

	^ self isUsed ifTrue: [ { self method } ] ifFalse: [ #(  ) ]
]

{ #category : #debugging }
LocalVariable >> write: aValue inContext: aContext [

	^self writeFromContext: aContext scope: aContext astScope value: aValue
]

{ #category : #debugging }
LocalVariable >> writeFromContext: aContext scope: contextScope value: aValue [

	| definitionContext |
	definitionContext := contextScope lookupDefiningContextForVariable: self startingFrom: aContext.
	^self writeFromLocalContext: definitionContext put: aValue
]

{ #category : #debugging }
LocalVariable >> writeFromLocalContext: aContext put: aValue [

	^ aContext tempAt: self index put: aValue
]
