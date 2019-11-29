"
A Mock object used in test
"
Class {
	#name : #DTMockPluginResult,
	#superclass : #DTPluginResult,
	#category : #'DrTests-Tests'
}

{ #category : #accessing }
DTMockPluginResult >> backgroundColor [
]

{ #category : #accessing }
DTMockPluginResult >> buildTreeForUI [
<pragmaForTest: 'for tests'>
	^ DTTreeNode new
		subResults: {};
		yourself
]
