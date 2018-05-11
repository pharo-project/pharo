"
SUnit tests for class SelectVistor

"
Class {
	#name : #SelectVisitorTest,
	#superclass : #AbstractEnumerationVisitorTest,
	#category : #'FileSystem-Tests-Core-Visitors'
}

{ #category : #tests }
SelectVisitorTest >> testBreadthFirst [
	| entries |
	entries := SelectVisitor breadthFirst: self root.
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/epsilon'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon/zeta'
			)
]

{ #category : #tests }
SelectVisitorTest >> testBreadthFirstSelect [
	| entries |
	entries := SelectVisitor breadthFirst: self root select: [ :node| node name endsWith: #a].
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon/zeta'
			)
]

{ #category : #tests }
SelectVisitorTest >> testPostorder [
	| entries |
	entries := SelectVisitor postorder: self root.
	self 
		assertEntries: entries
		are: #(
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/beta'
				'/alpha/epsilon/zeta'
				'/alpha/epsilon'
				'/alpha'
			)
]

{ #category : #tests }
SelectVisitorTest >> testPostorderSelect [
	| entries |
	entries := SelectVisitor postorder: self root select: [ :node| node name endsWith: #a].
	self 
		assertEntries: entries
		are: #(
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/beta'
				'/alpha/epsilon/zeta'
				'/alpha'
			)
]

{ #category : #tests }
SelectVisitorTest >> testPreorder [
	| entries |
	entries := SelectVisitor preorder: self root.
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon'
				'/alpha/epsilon/zeta'
			)
]

{ #category : #tests }
SelectVisitorTest >> testPreorderSelect [
	| entries |
	entries := SelectVisitor preorder: self root select: [ :node| node name endsWith: #a].
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon/zeta'
			)
]
