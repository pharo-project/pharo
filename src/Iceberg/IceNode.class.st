"
I am a tree implementation used for diffing and merging.

I work by composition and not inheritance. My users should use the values of the node (kept in the #value instance variable) and not subclass myself.
Each node stores its child nodes in a dictionary, indexed by a key.

Tree API:
 - #/ access to children
 - #path get the path of the current node in the tree
 - #parent get the parent node of the current node
 - #rootNode get the root node of the current tree
 - #children get the children of the current node
 - #keys, #keysDo:, #includesKey: enumerate the keys of the node
 - #at:put, #childAt:ifAbsentPut:, #childAt:ifPresent:
 - #resolve:, #resolve:ifAbsent: resolve a path in the current node
 - #replaceBy: replaces the current node by another node
 - #replace:with: replaces the given child node by another node

Enumerating API:
 - #allSatisfy:
 - #anySatisfy:
 - #collect:
 - #detect:
 - #select:
 - #count:
 - #do:

Visiting API:
 - visit:
"
Class {
	#name : #IceNode,
	#superclass : #Object,
	#instVars : [
		'parent',
		'cachedChanges',
		'childrenDictionary',
		'value'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #'instance creation' }
IceNode class >> value: aValue [
	
	^ self new
		value: aValue;
		yourself
]

{ #category : #'instance creation' }
IceNode class >> value: aValue parent: aParentNode [ 
	
	^ (self value: aValue)
		parent: aParentNode;
		yourself
]

{ #category : #accessing }
IceNode >> / aString [ 
	
	^ childrenDictionary at: aString
]

{ #category : #comparing }
IceNode >> = anotherNode [

	^ (self basicEqualsTo: anotherNode)
		and: [ self children asSet = anotherNode children asSet ]
]

{ #category : #visiting }
IceNode >> accept: aVisitor [
	
	^ aVisitor visitTreeNode: self
]

{ #category : #adding }
IceNode >> addChild: aChild [
	
	^ self addNode: (self class value: aChild).
]

{ #category : #adding }
IceNode >> addNode: aNode [

	self at: aNode key put: aNode.
	^ aNode
]

{ #category : #adding }
IceNode >> addTo: aTree [
	
	aTree addNode: self
]

{ #category : #enumerating }
IceNode >> allChildrenDo: aBlockClosure [ 
	
	self childrenDo: [ :e |
		aBlockClosure value: e.
		e allChildrenDo: aBlockClosure ]
]

{ #category : #accessing }
IceNode >> allPathSegments [

	self isRoot ifTrue: [ ^ #() ].
	self parent isRoot ifTrue: [ ^ { self key } ].
	^ self parent allPathSegments copyWith: self key
]

{ #category : #enumerating }
IceNode >> allSatisfy: aBlockClosure [ 
	
	(aBlockClosure value: self value)
		ifFalse: [ ^ false ].
		
	self childrenDo: [ :e |
		(e allSatisfy: aBlockClosure)
			ifFalse: [ ^ false ] ].

	^ true
]

{ #category : #enumerating }
IceNode >> anySatisfy: aBlockClosure [ 
	
	(aBlockClosure value: self value)
		ifTrue: [ ^ true ].
		
	self childrenDo: [ :e |
		(e anySatisfy: aBlockClosure)
			ifTrue: [ ^ true ] ].

	^ false
]

{ #category : #accessing }
IceNode >> at: aString put: aNode [ 
	
	aNode parent: self.
	childrenDictionary at: aString put: aNode
]

{ #category : #comparing }
IceNode >> basicEqualsTo: aNode [

	^ self value = aNode value
]

{ #category : #accessing }
IceNode >> childAt: aString ifAbsentPut: aBlock [
	
	^ childrenDictionary at: aString ifAbsent: [
		self addChild: aBlock value ]
]

{ #category : #accessing }
IceNode >> childAt: aString ifPresent: aBlockClosure [ 
	
	^ childrenDictionary at: aString ifPresent: aBlockClosure
]

{ #category : #accessing }
IceNode >> children [
	
	^ childrenDictionary values sorted: [ :a :b | a key < b key ]
]

{ #category : #enumerating }
IceNode >> childrenDo: aBlockClosure [ 
	
	self children do: aBlockClosure
]

{ #category : #enumerating }
IceNode >> collect: aBlock [
	| newNode |
	newNode := self class value: (aBlock value: self value).
	self nodesDo: [ :e |
		newNode addNode: (e collect: aBlock) ].
	^ newNode
]

{ #category : #enumerating }
IceNode >> collectWithPath: aBlock [
	| newNode |
	newNode := self class value: (aBlock value: self value value: self path).
	self nodesDo: [ :e |
		newNode addNode: (e collectWithPath: aBlock) ].
	^ newNode
]

{ #category : #enumerating }
IceNode >> count: aBlockClosure [ 
	
	| count |
	count := 0.
	(aBlockClosure value: self value)
		ifTrue: [ count := 1 ].
	
	self childrenDo: [ :each | count := count + (each count: aBlockClosure) ].
	^ count
]

{ #category : #accessing }
IceNode >> description [
	
	^ value description
]

{ #category : #enumerating }
IceNode >> detect: aBlock [
	^ (self firstNodeSuchThat: aBlock) value
]

{ #category : #enumerating }
IceNode >> do: aBlockClosure [ 
	
	aBlockClosure value: self value.
	self childrenDo: [ :node | node do: aBlockClosure ]
]

{ #category : #enumerating }
IceNode >> firstNodeSuchThat: conditionBlock [
	
	^ self firstNodeSuchThat: conditionBlock ifNone: [ NotFound signalFor: conditionBlock ]
]

{ #category : #enumerating }
IceNode >> firstNodeSuchThat: conditionBlock ifNone: noneBlock [
	
	(conditionBlock value: self)
		ifTrue: [ ^ self ].
	
	self
		nextNodeSuchThat: conditionBlock
		ifFound: [ :found | ^ found ].
	
	^ self parent
		ifNil: noneBlock
		ifNotNil: [
			self parent
				nextChildNodeStartingFrom: self
				suchThat: conditionBlock
				ifNone: noneBlock ]
]

{ #category : #testing }
IceNode >> hasChanges [
	
	^ value hasChanges or: [ self children anySatisfy: [ :child | child hasChanges ] ]
]

{ #category : #comparing }
IceNode >> hash [

	^ self key hash
]

{ #category : #accessing }
IceNode >> includesKey: anObject [
	
	^ childrenDictionary includesKey: anObject
]

{ #category : #initialization }
IceNode >> initialize [

	super initialize.
	childrenDictionary := Dictionary new
]

{ #category : #testing }
IceNode >> isEmpty [
	
	^ childrenDictionary isEmpty
]

{ #category : #testing }
IceNode >> isEmptyNode [

	^ false
]

{ #category : #testing }
IceNode >> isRoot [
	
	^ self parent isNil
]

{ #category : #accessing }
IceNode >> key [
	
	^ value key
]

{ #category : #accessing }
IceNode >> keys [

	^ childrenDictionary keys
]

{ #category : #accessing }
IceNode >> keysDo: aBlockClosure [ 
	
	childrenDictionary keysDo: aBlockClosure
]

{ #category : #enumerating }
IceNode >> mergeWith: anotherTree onLeft: leftBlock onRight: rightBlock onMerge: mergeBlock [
	
	| mergedTree newNode |
	mergedTree := mergeBlock value: self value value: anotherTree value.
	newNode := self class value: mergedTree.
		
	self keysDo: [ :key |
		(anotherTree includesKey: key)
			ifTrue:  [ | childNode | 
				childNode := (self / key)
					mergeWith: anotherTree / key
					onLeft: leftBlock
					onRight: rightBlock
					onMerge: mergeBlock.
				childNode addTo: newNode ]
			ifFalse: [ 
				newNode addNode: ((self / key) collect: leftBlock) ] ].

	anotherTree keysDo: [ :key |
		(self includesKey: key)
			ifFalse: [ 
				newNode addNode: ((anotherTree / key) collect: rightBlock) ] ].

	^ newNode
]

{ #category : #enumerating }
IceNode >> mergeWith: anotherTree onMerge: mergeBlock [
	
	^ self
		mergeWith: anotherTree
		onLeft: [ :each | each ]
		onRight:  [ :each | each ]
		onMerge: mergeBlock
]

{ #category : #enumerating }
IceNode >> nextChildNodeStartingFrom: anIceNode suchThat: conditionBlock ifNone: noneBlock [ 
	
	| aCollection |
	aCollection := self children.
	(aCollection indexOf: anIceNode) + 1
		to: aCollection size do: [ :index | | item |
			item := aCollection at: index.
			(conditionBlock value: item)
				ifTrue: [ ^ item ].
			item
				nextNodeSuchThat: conditionBlock
				ifFound: [ :found | ^ found ] ].
	
	self parent ifNil: [^ noneBlock value ].
	
	^ self parent
		nextChildNodeStartingFrom: self
		suchThat: conditionBlock
		ifNone: noneBlock
]

{ #category : #enumerating }
IceNode >> nextNodeSuchThat: conditionBlock ifFound: foundBlock [ 
	
	self children do: [ :each |
		(conditionBlock value: each)
			ifTrue: [ ^ foundBlock value: each ].
		each
			nextNodeSuchThat: conditionBlock
			ifFound: [ :found | ^  foundBlock value: found  ] ].
]

{ #category : #enumerating }
IceNode >> nextNodeSuchThat: conditionBlock ifNone: noneBlock [
	
	self children do: [ :each |
		(conditionBlock value: each)
			ifTrue: [ ^ each ].
		each
			nextNodeSuchThat: conditionBlock
			ifFound: [ :found | ^ found  ] ].
	
	^ self parent
		nextChildNodeStartingFrom: self
		suchThat: conditionBlock
		ifNone: noneBlock
]

{ #category : #enumerating }
IceNode >> nodesDo: aBlockClosure [ 
	
	childrenDictionary valuesDo: aBlockClosure
]

{ #category : #testing }
IceNode >> notEmpty [
	
	^ childrenDictionary notEmpty
]

{ #category : #accessing }
IceNode >> parent [
	^ parent
]

{ #category : #accessing }
IceNode >> parent: anObject [
	
	parent := anObject
]

{ #category : #accessing }
IceNode >> path [
	
	^ RelativePath withAll: self allPathSegments
]

{ #category : #printing }
IceNode >> printOn: aStream [
	aStream
		nextPutAll: 'IceNode(';
		nextPutAll: self key asString;
		nextPutAll: ')'
]

{ #category : #accessing }
IceNode >> replace: oldNode with: newNode [
	
	self assert: oldNode name = newNode name.
	childrenDictionary at: oldNode name put: newNode
]

{ #category : #accessing }
IceNode >> replaceBy: aNode [ 
	
	parent replace: self with: aNode
]

{ #category : #accessing }
IceNode >> resolve: aRelativePath [ 
	
	^ self resolve: aRelativePath ifAbsent: [ NotFound signalFor: aRelativePath ]
]

{ #category : #accessing }
IceNode >> resolve: aRelativePath ifAbsent: aBlock [
	
	^ self resolve: aRelativePath ifPresent: [ :each | each ] ifAbsent: aBlock
]

{ #category : #accessing }
IceNode >> resolve: aRelativePath ifPresent: presentBlock ifAbsent: absentBlock [ 	
	| segments node |
	segments := aRelativePath segments.
	
	segments ifEmpty: [ ^ presentBlock value: self value ].
	
	node := childrenDictionary at: segments first ifAbsent: [^ absentBlock value ].
	
	^node
		resolve: (RelativePath withAll: segments allButFirst) 
		ifPresent: presentBlock
		ifAbsent: absentBlock
]

{ #category : #accessing }
IceNode >> rootNode [
	
	^ self parent rootNode
]

{ #category : #enumerating }
IceNode >> select: aBlock [

	| selectedChildren newNode |
	selectedChildren := self children collect: [ :childNode | 
		childNode select: aBlock ].
	
	((selectedChildren anySatisfy: [ :each | each isEmptyNode not ])
		or: [ aBlock value: self value ])
			ifFalse: [ ^ IceEmptyNode new ].
		
	newNode := self class value: self value.
	selectedChildren do: [ :e |
		e addTo: newNode ].
	^ newNode
]

{ #category : #evaluating }
IceNode >> value [
	
	^ value
]

{ #category : #evaluating }
IceNode >> value: aValue [
 
	value := aValue
]
