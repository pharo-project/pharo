"
SUnit tests for class CollectVisitor
"
Class {
	#name : 'CollectVisitorTest',
	#superclass : 'AbstractEnumerationVisitorTest',
	#category : 'FileSystem-Core-Tests-Visitors',
	#package : 'FileSystem-Core-Tests',
	#tag : 'Visitors'
}

{ #category : 'tests' }
CollectVisitorTest >> testBreadthFirst [
	| entries |
	entries := CollectVisitor breadthFirst: self root.
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

{ #category : 'tests' }
CollectVisitorTest >> testPostorder [
	| entries |
	entries := CollectVisitor postorder: self root.
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

{ #category : 'tests' }
CollectVisitorTest >> testPreorder [
	| entries |
	entries := CollectVisitor preorder: self root.
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
