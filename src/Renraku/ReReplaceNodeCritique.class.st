"
A critique which know which node should be replaced by another one. These critiques are used first of all in rewrite rules.
"
Class {
	#name : 'ReReplaceNodeCritique',
	#superclass : 'ReAbstractCritique',
	#instVars : [
		'oldNode',
		'newNode'
	],
	#category : 'Renraku-Critiques',
	#package : 'Renraku',
	#tag : 'Critiques'
}

{ #category : 'instance creation' }
ReReplaceNodeCritique class >> rule: aRule achor: anAnchor oldNode: aNode newNode: anotherNode [

	^ self basicNew
		initializeRule: aRule
		sourceAnchor: anAnchor
		oldNode: aNode
		newNode: anotherNode
]

{ #category : 'accessing' }
ReReplaceNodeCritique >> change [
    "Parse a fresh AST, then remove or replace the matched node.
    Generate an add-method change from the updated AST."

    | newTree newTreeNode parent |
    newTree := self entity parseTree.
    newTreeNode := newTree nodeAtTraversalIndex: (
        oldNode methodNode traversalIndexOf: oldNode).
    self assert: newTreeNode = oldNode.

    parent := newTreeNode parent.
    (newNode isSequence and: [
        newNode statements isEmpty and: [
            newNode temporaries isEmpty and: [
                parent isNotNil and: [
                    parent isSequence and: [
                        parent statements identityIncludes: newTreeNode ] ] ] ] ])
        ifTrue: [ parent removeNode: newTreeNode ]
        ifFalse: [
            newTreeNode replaceMethodSource: newNode.
            newTreeNode becomeForward: newNode ].

    ^ ReChangeFactory instance
        addMethodSource: newTree methodNode newSource
        in: self entity methodClass
]

{ #category : 'initialization' }
ReReplaceNodeCritique >> initializeRule: aRule sourceAnchor: anAnchor oldNode: aNode newNode: anotherNode [

	self initializeRule: aRule sourceAnchor: anAnchor.
	oldNode := aNode.
	newNode := anotherNode
]

{ #category : 'testing' }
ReReplaceNodeCritique >> providesChange [

	^ self entity isCompiledMethod
]
