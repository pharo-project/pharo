"
I map how instance specific links are put on nodes, in different ways.

I provide an interface to easily and fastly find a links for a given node or nodes for a given link.
"
Class {
	#name : #MetaLinkNodesMapper,
	#superclass : #Object,
	#instVars : [
		'nodesForObjects',
		'objectsForNodes'
	],
	#category : #'Reflectivity-Installer'
}

{ #category : #mapping }
MetaLinkNodesMapper >> addNode: aNode forObject: anObject [
	(nodesForObjects at: anObject ifAbsentPut: [ IdentitySet new ]) add: aNode.
	(objectsForNodes at: aNode ifAbsentPut: [ WeakSet new ]) add: anObject.
	^ aNode
]

{ #category : #mapping }
MetaLinkNodesMapper >> findNodeLike: aNode forObject: anObject [
	| nodesForObject |
	nodesForObject := nodesForObjects at: anObject ifAbsent: [ #() ].
	^ nodesForObject detect: [ :node | node isEquivalentTo: aNode ] ifNone: [ nil ]
]

{ #category : #mapping }
MetaLinkNodesMapper >> findNodesForObject: anObject [
	^ nodesForObjects at: anObject ifAbsent: [ #() ]
]

{ #category : #initialize }
MetaLinkNodesMapper >> initialize [
	objectsForNodes := WeakKeyDictionary new.
	nodesForObjects := WeakKeyDictionary new
]

{ #category : #mapping }
MetaLinkNodesMapper >> isNode: aNode ownedBy: anObject [
	^(nodesForObjects at: anObject ifAbsent:[^false]) includes: aNode
]

{ #category : #mapping }
MetaLinkNodesMapper >> removeNode: aNode forObject: anObject [
	"I am just a helper object, if someone asks me to remove a node or an object
	that i don't know, i don't raise any errors"
	(nodesForObjects at: anObject) remove: aNode ifAbsent:[].
	(objectsForNodes at: aNode) remove: anObject ifAbsent:[]
]
