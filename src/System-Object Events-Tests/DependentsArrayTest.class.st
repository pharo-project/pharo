"
SUnit tests for class  DependentsArray
"
Class {
	#name : 'DependentsArrayTest',
	#superclass : 'TestCase',
	#category : 'System-Object Events-Tests',
	#package : 'System-Object Events-Tests'
}

{ #category : 'tests' }
DependentsArrayTest >> testSize [
	self
		assert: (DependentsArray with: nil) size equals: 0;
		assert: (DependentsArray with: nil with: 1 with: nil) size equals: 1;
		assert: (DependentsArray with: 1 with: 3) size equals: 2;
		assert: (DependentsArray with: nil with: nil with: nil) size equals: 0
]
