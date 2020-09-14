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
	widget := SpStringTableColumn basicNew.
	self assert: widget isSortable.
	widget isSortable: false.
	self deny: widget isSortable
]
