"
My job is to resolve targets for metalinks to be put on. I only return the target(s) i resolved for the given parameters, which could be: 
- a collection of ast nodes
- a slot
- a temporary variable 
- a literal variable

Some methods have an option parameter. This is the case for links to be put on slots, temporaries and literal variables.
The option can be one of the following:
#all - will lookup for all access nodes.
#read  - will lookup for read nodes only. 
#write - will lookup for write nodes only (i.e. assignment nodes) .

I do not take care that methods, slots or globals do exist. Users must ensure they provide valid parameters and/or to handle possible errors.

I only have utility methods in class side.
"
Class {
	#name : #MetalinkTargetResolver,
	#superclass : #Object,
	#category : #'Reflectivity-Installer'
}

{ #category : #'resolving - private' }
MetalinkTargetResolver class >> assignmentNodesFor: aVarOrSlot [
	(aVarOrSlot isKindOf: Slot)
		ifFalse: [ ^ aVarOrSlot assignmentNodes asIdentitySet ].
	^ ((self
		usingNodesForVariable: aVarOrSlot
		in: aVarOrSlot owningClass withAllSubclasses)
		select: [ :each | each isWrite ]
		thenCollect: [ :each | each parent ]) asIdentitySet
]

{ #category : #resolving }
MetalinkTargetResolver class >> classFor: aClassOrObject [
	| class |
	aClassOrObject isClass
		ifTrue: [ ^ aClassOrObject ].
	class := aClassOrObject class.
	^ class isAnonymous
		ifTrue: [ class superclass ]
		ifFalse: [ class ]
]

{ #category : #api }
MetalinkTargetResolver class >> classVariableNamed: aClassVariableName for: aClassOrObject [
	^ self lookupClassVariableNamed: aClassVariableName asSymbol in: aClassOrObject
]

{ #category : #api }
MetalinkTargetResolver class >> classVariableNamed: aClassVariableName for: aClassOrObject option: option [
	| var |
	var := self lookupClassVariableNamed: aClassVariableName asSymbol in: aClassOrObject.
	^ self resolveASTLookupFor: var option: option
]

{ #category : #resolving }
MetalinkTargetResolver class >> lookupASTForMethodNamed: aMethodName in: aClassOrObject [
	| class |
	class := self classFor: aClassOrObject.
	^ (class lookupSelector: aMethodName) ast
]

{ #category : #resolving }
MetalinkTargetResolver class >> lookupClassVariableNamed: aClassVariableName in: aClassOrObject [
	^ (self classFor: aClassOrObject) classVariableNamed: aClassVariableName
]

{ #category : #resolving }
MetalinkTargetResolver class >> lookupSlotNamed: aSlotName in: aClassOrObject [
	| class |
	class := self classFor: aClassOrObject.
	^ class slotNamed: aSlotName
]

{ #category : #resolving }
MetalinkTargetResolver class >> lookupTemporaryNamed: aTempVarName inMethod: aMethodName for: aClassOrObject [
	| class method |
	class := self classFor: aClassOrObject.
	method := class lookupSelector: aMethodName.
	^ method temporaryVariableNamed: aTempVarName
]

{ #category : #api }
MetalinkTargetResolver class >> methodNamed: aMethodName for: aClassOrObject [
	^ self lookupASTForMethodNamed: aMethodName asSymbol in: aClassOrObject
]

{ #category : #'resolving - private' }
MetalinkTargetResolver class >> readNodesFor: aVarOrSlot [
	(aVarOrSlot isKindOf: Slot)
		ifFalse: [ ^ aVarOrSlot readNodes asIdentitySet ].
	^ ((self
		usingNodesForVariable: aVarOrSlot
		in: aVarOrSlot owningClass withAllSubclasses)
		select: [ :each | each isRead ]) asIdentitySet
]

{ #category : #resolving }
MetalinkTargetResolver class >> resolveASTLookupFor: varOrSlot option: option [
	^ (self resolvingBlocForOption: option) value: varOrSlot
]

{ #category : #'resolving - private' }
MetalinkTargetResolver class >> resolvingBlocForOption: option [
	option = #write
		ifTrue: [ ^ [ :varOrSlot | self assignmentNodesFor: varOrSlot ] ].
	option = #read
		ifTrue: [ ^ [ :varOrSlot | self readNodesFor: varOrSlot ] ].
	^ [ :varOrSlot | 
	(OrderedCollection
		with: (self assignmentNodesFor: varOrSlot)
		with: (self readNodesFor: varOrSlot)) flattened asIdentitySet ]
]

{ #category : #api }
MetalinkTargetResolver class >> slotNamed: aSlotName for: aClassOrObject [
	^ self lookupSlotNamed: aSlotName asSymbol in: aClassOrObject
]

{ #category : #api }
MetalinkTargetResolver class >> slotNamed: aSlotName for: aClassOrObject option: option [
	| slot |
	slot := self lookupSlotNamed: aSlotName asSymbol in: aClassOrObject.
	^ self resolveASTLookupFor: slot option: option
]

{ #category : #api }
MetalinkTargetResolver class >> slotOrVarNodesFor: slotOrVar inMethod: method option: option [
	| nodes |
	nodes := (self resolvingBlocForOption: option) value: slotOrVar.
	^ nodes select: [ :node | node methodNode method = method ]
]

{ #category : #api }
MetalinkTargetResolver class >> temporaryNamed: aTempVarName inMethod: aMethodName for: aClassOrObject [
	^ self lookupTemporaryNamed: aTempVarName asSymbol inMethod: aMethodName for: aClassOrObject
]

{ #category : #api }
MetalinkTargetResolver class >> temporaryNamed: aTempVarName inMethod: aMethodName for: aClassOrObject option: option [
	| temp |
	temp := self lookupTemporaryNamed: aTempVarName asSymbol inMethod: aMethodName for: aClassOrObject.
	^ self resolveASTLookupFor: temp option: option
]

{ #category : #'resolving - private' }
MetalinkTargetResolver class >> usingNodesForVariable: aVarOrSlot in: classes [
	^ (classes flatCollect: [ :c | c variableNodes ])
		select: [ :each | each binding variable == aVarOrSlot ]
]
