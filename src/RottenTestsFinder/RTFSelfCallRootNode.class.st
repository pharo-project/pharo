"
I model a node in the call tree built from a CompiledMethod.
I specifically model the root node of a call tree.

For this purpose, I store
- #compiledMethod : The CompiledMethod called.
- #subCalls : A collection of selfcall nodes.
"
Class {
	#name : 'RTFSelfCallRootNode',
	#superclass : 'Object',
	#instVars : [
		'compiledMethod',
		'subCalls'
	],
	#category : 'RottenTestsFinder-SelfInterpreter',
	#package : 'RottenTestsFinder',
	#tag : 'SelfInterpreter'
}

{ #category : 'instance creation' }
RTFSelfCallRootNode class >> compiledMethod: aCompiledMethod [
	^ self new
		compiledMethod: aCompiledMethod;
		yourself
]

{ #category : 'visiting' }
RTFSelfCallRootNode >> acceptVisitor: aRTFSelfCallVisitor [
	^ aRTFSelfCallVisitor visitRTFSelfCallRootNode: self
]

{ #category : 'adding' }
RTFSelfCallRootNode >> addCall: aRTFSelfCallNode [
	self subCalls add: aRTFSelfCallNode
]

{ #category : 'accessing' }
RTFSelfCallRootNode >> allCompiledMethods [
	"Returns all compiled methods in the call tree."
	| visitor |
	visitor := RTFMethodCallsCollector new.
	self acceptVisitor: visitor.
	^ visitor compiledMethods
]

{ #category : 'testing' }
RTFSelfCallRootNode >> canLeadToAssertPrimitive [
	^ self acceptVisitor: RTFLeadsToAssertPrimitiveCallChecker new
]

{ #category : 'cleaning' }
RTFSelfCallRootNode >> cleanSubTreesNotLeadingToAssertPrimitive [
	self acceptVisitor: RTFSelfCallTreeCleaner new.
	^ self
]

{ #category : 'accessing' }
RTFSelfCallRootNode >> compiledMethod [
	^ compiledMethod
]

{ #category : 'accessing' }
RTFSelfCallRootNode >> compiledMethod: anObject [
	compiledMethod := anObject
]

{ #category : 'accessing' }
RTFSelfCallRootNode >> helperCompiledMethodCalled [
	| visitor |
	visitor := RTFHelperCallsCollector new.
	self acceptVisitor: visitor.
	^ visitor helperCalls
]

{ #category : 'accessing' }
RTFSelfCallRootNode >> helperMaxDepth [
	^ self acceptVisitor: RTFMaxHelperDepthComputer new
]

{ #category : 'initialization' }
RTFSelfCallRootNode >> initialize [
	super initialize.
	self subCalls: OrderedCollection new
]

{ #category : 'metalinks' }
RTFSelfCallRootNode >> installMetalinks [
	"Recursively install metalinks on all subnodes of the tree."
	self acceptVisitor: RTFRecursiveMetalinksInstaller new
]

{ #category : 'testing' }
RTFSelfCallRootNode >> isCallToAssertPrimitive [
	"Returns true if the CompiledMethod I hold is an assertion primitive."
	^ RottenTestsFinder assertionCompiledMethods includes: self compiledMethod
]

{ #category : 'accessing' }
RTFSelfCallRootNode >> primitiveCompiledMethodCalled [
	| visitor |
	visitor := RTFPrimitiveCallsCollector new.
	self acceptVisitor: visitor.
	^ visitor primitiveCalls
]

{ #category : 'printing' }
RTFSelfCallRootNode >> printOn: aStream [
	super printOn: aStream.
	aStream
		<< $(.
	self compiledMethod printOn: aStream.
	aStream
		<< $)
]

{ #category : 'accessing' }
RTFSelfCallRootNode >> subCalls [
	^ subCalls
]

{ #category : 'accessing' }
RTFSelfCallRootNode >> subCalls: anObject [
	subCalls := anObject
]

{ #category : 'metalinks' }
RTFSelfCallRootNode >> uninstallMetalinks [
	"Recursively uninstall metalinks on all subnodes of the tree."
	self acceptVisitor: RTFRecursiveMetalinksUninstaller new
]
