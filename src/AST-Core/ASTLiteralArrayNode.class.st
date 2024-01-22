"
An RBLiteralArrayNode is an AST node that represents literal arrays #(1 2 3) and literal byte arrays #[ 1 2 3 ].

Instance Variables
	contents: <Array of: RBLiteralNode> literal nodes of the array
	isByteArray: <Boolean> if the receiver is a literal byte array

"
Class {
	#name : 'ASTLiteralArrayNode',
	#superclass : 'ASTLiteralNode',
	#instVars : [
		'isByteArray',
		'contents',
		'openerSize'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'class initialization' }
ASTLiteralArrayNode class >> initialize [
	self deprecatedAliases: { #RBLiteralArrayNode }
]

{ #category : 'instance creation' }
ASTLiteralArrayNode class >> startPosition: startInteger contents: anArray stopPosition: stopInteger isByteArray: aBoolean [
	^(self new)
		startPosition: startInteger
			contents: anArray
			stopPosition: stopInteger
			isByteArray: aBoolean;
		yourself
]

{ #category : 'instance creation' }
ASTLiteralArrayNode class >> value: aValue [
	^(self new)
		startPosition: 0
			contents: (aValue asArray collect: [:each | ASTLiteralNode value: each])
			stopPosition: -1
			isByteArray: aValue class ~~ Array;
		yourself
]

{ #category : 'comparing' }
ASTLiteralArrayNode >> = anObject [
	super = anObject ifFalse: [^false].
	self isForByteArray = anObject isForByteArray ifFalse: [^false].
	self contents size = anObject contents size ifFalse: [^false].
	self contents
		with: anObject contents
		do: [:first :second | first = second ifFalse: [^false]].
	^true
]

{ #category : 'visiting' }
ASTLiteralArrayNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitLiteralArrayNode: self
]

{ #category : 'accessing' }
ASTLiteralArrayNode >> children [
	^ contents
]

{ #category : 'accessing' }
ASTLiteralArrayNode >> contents [
	^contents
]

{ #category : 'initialization' }
ASTLiteralArrayNode >> contents: aRBLiteralNodeCollection [
	contents := aRBLiteralNodeCollection.
	contents do: [:each | each parent: self]
]

{ #category : 'matching' }
ASTLiteralArrayNode >> copyInContext: aDictionary [
	^ self class
		startPosition: 0
		contents: (self copyList: self contents inContext: aDictionary)
		stopPosition: -1
		isByteArray: isByteArray
]

{ #category : 'comparing' }
ASTLiteralArrayNode >> equalTo: anObject withMapping: aDictionary [
	self class = anObject class ifFalse: [^false].
	self isForByteArray = anObject isForByteArray ifFalse: [^false].
	self contents size = anObject contents size ifFalse: [^false].
	self contents
		with: anObject contents
		do: [:first :second | (first equalTo: second withMapping: aDictionary) ifFalse: [^false]].
	^true
]

{ #category : 'errors' }
ASTLiteralArrayNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^self contents anySatisfy: [:each | each isFaulty]
]

{ #category : 'testing' }
ASTLiteralArrayNode >> isForByteArray [
	^isByteArray
]

{ #category : 'testing' }
ASTLiteralArrayNode >> isLiteralArray [
	^true
]

{ #category : 'matching' }
ASTLiteralArrayNode >> match: aNode inContext: aDictionary [
	aNode class = self class ifFalse: [^false].
	self isForByteArray = aNode isForByteArray ifFalse: [^false].
	^self
		matchList: contents
		against: aNode contents
		inContext: aDictionary
]

{ #category : 'accessing' }
ASTLiteralArrayNode >> openerSize [

	^ openerSize
]

{ #category : 'accessing' }
ASTLiteralArrayNode >> openerSize: anObject [

	openerSize := anObject
]

{ #category : 'copying' }
ASTLiteralArrayNode >> postCopy [
	super postCopy.
	self contents: (self contents collect: [ :each | each copy ])
]

{ #category : 'replacing' }
ASTLiteralArrayNode >> replaceNode: aNode withNode: anotherNode [
	self contents: (contents
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])
]

{ #category : 'private - replacing' }
ASTLiteralArrayNode >> replaceSourceWith: aNode [
	(self class = aNode class and:
			[self isForByteArray = aNode isForByteArray
				and: [self contents size = aNode contents size]])
		ifFalse: [^super replaceSourceWith: aNode].
	self contents
		with: aNode contents
		do: [:first :second | first = second ifFalse: [ first replaceSourceWith: second]]
]

{ #category : 'initialization' }
ASTLiteralArrayNode >> startPosition: startInteger contents: anArray stopPosition: stopInteger isByteArray: aBoolean [
	start := startInteger.
	self contents: anArray.
	stop := stopInteger.
	isByteArray := aBoolean
]

{ #category : 'accessing' }
ASTLiteralArrayNode >> value [
	| array |
	array := (isByteArray ifTrue: [ByteArray] ifFalse: [Array])
				new: contents size.
	1 to: contents size
		do: [:each | array at: each put: (contents at: each) value].
	^array
]
