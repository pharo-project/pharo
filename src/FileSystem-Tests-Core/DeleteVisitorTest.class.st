"
SUnit tests for class DeleteVisitor
"
Class {
	#name : #DeleteVisitorTest,
	#superclass : #SingleTreeTest,
	#category : #'FileSystem-Tests-Core-Visitors'
}

{ #category : #tests }
DeleteVisitorTest >> testBeta [
	self setUpGreek.
	DeleteVisitor delete: (filesystem / 'alpha' / 'beta').
	self assert: (filesystem isDirectory: '/alpha').
	self assert: (filesystem isDirectory: '/alpha/epsilon').
	self deny: (filesystem exists: '/alpha/beta')
	
]
