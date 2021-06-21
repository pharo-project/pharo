Class {
	#name : #VirtualBreakpoint,
	#superclass : #Object,
	#instVars : [
		'node',
		'setter',
		'breakOnce',
		'whenHit'
	],
	#classVars : [
		'all'
	],
	#category : #'Sindarin-Utilities'
}

{ #category : #'instance creation' }
VirtualBreakpoint class >> all [

	all ifNil: [ all := OrderedCollection new ].
	^ all
]

{ #category : #actions }
VirtualBreakpoint class >> clearAll [
	<script>
	all := nil
]

{ #category : #'instance creation' }
VirtualBreakpoint class >> newOnNode: aNode [
	"Used by ScriptableDebugger. Should be removed in favour of newOnNode:setBy: if the latter stays"

	^ self new
		  node: aNode;
		  yourself
]

{ #category : #'instance creation' }
VirtualBreakpoint class >> newOnNode: aNode setBy: anObject [

	| breakPoint |
	breakPoint := self new node: aNode; setter: anObject.
	self all add: breakPoint.
	^ breakPoint
]

{ #category : #accessing }
VirtualBreakpoint >> breakOnce [

	breakOnce ifNil: [ breakOnce := false ].
	^ breakOnce
]

{ #category : #accessing }
VirtualBreakpoint >> breakOnce: anObject [

	breakOnce := anObject
]

{ #category : #hitTest }
VirtualBreakpoint >> hitTest: aNode [
	"aNode is assumed to be the current node of the execution. If aNode is equal to the node on which this breakpoint has been set, evaluate all the blocks in whenHit and return true. Otherwise return false."

	self node == aNode ifTrue: [ 
		self breakOnce ifTrue: [ self remove ].
		self whenHit do: [ :block | block value ].
		^ true ].
	^ false
]

{ #category : #accessing }
VirtualBreakpoint >> node [

	^ node
]

{ #category : #accessing }
VirtualBreakpoint >> node: anObject [

	node := anObject
]

{ #category : #API }
VirtualBreakpoint >> once [
	"Configures this breakpoint to remove itself the next time it is hit"

	breakOnce := true
]

{ #category : #API }
VirtualBreakpoint >> remove [

	all remove: self
]

{ #category : #accessing }
VirtualBreakpoint >> setter [

	^ setter
]

{ #category : #accessing }
VirtualBreakpoint >> setter: anObject [

	setter := anObject
]

{ #category : #accessing }
VirtualBreakpoint >> whenHit [

	whenHit ifNil: [ whenHit := OrderedCollection new ].
	^ whenHit
]

{ #category : #API }
VirtualBreakpoint >> whenHit: aBlock [
	"Sets aBlock to be evaluated when this breakpoint is hit. If this method is called multiple times, all the blocks provided will be evaluated when this breakpoint is hit."

	self whenHit add: aBlock
]
