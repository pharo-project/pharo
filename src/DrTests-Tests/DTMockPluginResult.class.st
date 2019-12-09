"
A Mock object used in test
"
Class {
	#name : #DTMockPluginResult,
	#superclass : #DTPluginResult,
	#category : #'DrTests-Tests'
}

{ #category : #accessing }
DTMockPluginResult >> buildAnotherTreeForUI [
	<pragmaForTest: 'for other tests' order: 2>
	^ DTTreeNode new
		subResults: {};
		yourself
]

{ #category : #accessing }
DTMockPluginResult >> buildTreeForUI [
	<pragmaForTest: 'for tests' order: 1>
	^ DTTreeNode new
		subResults: {};
		yourself
]
