"
An RBComment represents a text comment associated with an AST node.

An RBComment is not an AST-Node (not a subclass of program node). But its instances are just wrapping the comment text and (start-) position.

Due to the way the parser handles comments, the RBComment is assigned to its preceding (real) AST node, although we often write the comment prior to a statement.

For example:

foo
""method comment""

self firstStatement.

""comment about the return""
^ self

The ""method comment"" is assigned to the method node, the ""comment about the return"" is assigned
to the ""self firstStatement"" node!

instance variables
	contents 	<String> the comment text
	start	<Number> (start-) position within the method source

"
Class {
	#name : #RBComment,
	#superclass : #RBProgramNode,
	#instVars : [
		'contents',
		'start'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
RBComment class >> with: aString at: startPosition [
	^ self new
		with: aString at: startPosition;
		yourself. 
	
]

{ #category : #visiting }
RBComment >> acceptVisitor: aProgramNodeVisitor [
	"At some point we will have to think what we do to visit comment. 
	It may have an impact on visitors so this should be done carefully.
	Since by default previously comment node were not subclass of ProgramNode 
	we do nothing by default."

	
]

{ #category : #accessing }
RBComment >> contents [
	^ contents
]

{ #category : #properties }
RBComment >> hasProperty: aKey [
	"Test if the property aKey is present."
	
	^ properties notNil and: [ properties includesKey: aKey ]
]

{ #category : #testing }
RBComment >> intersectsInterval: anInterval [ 
	"Make comments polymorphic with program nodes for hit detection"
	
	^(anInterval first between: self start and: self stop) 
		or: [ self start between: anInterval first and: anInterval last ]
]

{ #category : #accessing }
RBComment >> parent [
	^ parent
]

{ #category : #accessing }
RBComment >> parent: anObject [
	parent := anObject
]

{ #category : #printing }
RBComment >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: ' "'.
	aStream nextPutAll: contents.
	aStream nextPutAll: '" '.
]

{ #category : #properties }
RBComment >> propertyAt: aKey [
	"Answer the property value associated with aKey."
	
	^ self propertyAt: aKey ifAbsent: [ self error: 'Property not found' ]
]

{ #category : #properties }
RBComment >> propertyAt: aKey ifAbsent: aBlock [
	"Answer the property value associated with aKey or, if aKey isn't found, answer the result of evaluating aBlock."
	
	^ properties 
		ifNil: aBlock
		ifNotNil: [ properties at: aKey ifAbsent: aBlock ]
]

{ #category : #properties }
RBComment >> propertyAt: aKey ifAbsentPut: aBlock [
	"Answer the property associated with aKey or, if aKey isn't found store the result of evaluating aBlock as new value."
	
	^ self propertyAt: aKey ifAbsent: [ self propertyAt: aKey put: aBlock value ]
]

{ #category : #properties }
RBComment >> propertyAt: aKey ifPresent: aPresentBlock ifAbsent: anAbsentBlock [
	"Answer the property value associated with aKey or, if aKey is found, answer the result of evaluating aPresentBlock, else evaluates anAbsentBlock."

	^ properties ifNil: anAbsentBlock ifNotNil: [ properties at: aKey ifPresent: aPresentBlock ifAbsent: anAbsentBlock ]
]

{ #category : #properties }
RBComment >> propertyAt: aKey put: anObject [
	"Set the property at aKey to be anObject. If aKey is not found, create a new entry for aKey and set is value to anObject. Answer anObject."

	^ (properties ifNil: [ properties := SmallDictionary new: 1 ])
		at: aKey put: anObject
]

{ #category : #properties }
RBComment >> removeProperty: aKey [
	"Remove the property with aKey. Answer the property or raise an error if aKey isn't found."
	
	^ self removeProperty: aKey ifAbsent: [ self error: 'Property not found' ].
]

{ #category : #properties }
RBComment >> removeProperty: aKey ifAbsent: aBlock [
	"Remove the property with aKey. Answer the value or, if aKey isn't found, answer the result of evaluating aBlock."
	
	| answer |
	properties ifNil: [ ^ aBlock value ].
	answer := properties removeKey: aKey ifAbsent: aBlock.
	properties isEmpty ifTrue: [ properties := nil ].
	^ answer
]

{ #category : #accessing }
RBComment >> size [
	^ contents size + 2 "must take into account quotation marks"
]

{ #category : #accessing }
RBComment >> start [
	^ start
]

{ #category : #accessing }
RBComment >> stop [
	^ start + self size - 1
]

{ #category : #initialization }
RBComment >> with: aString at: startPosition [
	start := startPosition.
	contents := aString

]
