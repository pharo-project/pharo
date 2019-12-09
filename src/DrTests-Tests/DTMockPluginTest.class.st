"
A DTMockPluginTest is a test class for testing the behavior of DTMockPlugin
"
Class {
	#name : #DTMockPluginTest,
	#superclass : #TestCase,
	#category : #'DrTests-Tests'
}

{ #category : #tests }
DTMockPluginTest >> testResultTreeViews [
	| plugin treeViews |
	plugin := DTMockPlugin new.
	
	treeViews := plugin resultTreeViews.
	
	self assert: treeViews size equals: 2.
	
	self assert: treeViews first name equals: 'for tests'.
	self assert: treeViews second name equals: 'for other tests'.
]
