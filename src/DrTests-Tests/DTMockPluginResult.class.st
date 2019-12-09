"
A Mock object used in test
"
Class {
	#name : #DTMockPluginResult,
	#superclass : #DTPluginResult,
	#category : #'DrTests-Tests'
}

{ #category : #accessing }
DTMockPluginResult >> buildTreeForUI [
<pragmaForTest: 'for tests' order: 1>
	^ DTTreeNode new
		subResults: {};
		yourself
]
