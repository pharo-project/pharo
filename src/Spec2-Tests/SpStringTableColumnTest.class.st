"
A SpStringTableColumnTest is a test class for testing the behavior of SpStringTableColumn
"
Class {
	#name : #SpStringTableColumnTest,
	#superclass : #TestCase,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #tests }
SpStringTableColumnTest >> testIsSortable [
	|widget|
	widget := SpStringTableColumn new.
	self deny: widget isSortable.
	widget sortFunction: [:a :b|].
	self assert: widget isSortable.
	widget sortFunction: nil.
	self deny: widget isSortable
]
