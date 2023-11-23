"
A Mock object used in test
"
Class {
	#name : 'DTMockPluginResult',
	#superclass : 'DTPluginResult',
	#category : 'DrTests-Tests',
	#package : 'DrTests-Tests'
}

{ #category : 'accessing' }
DTMockPluginResult >> backgroundColor [
  " Backgournd color, although untested is used by updateResultLabel, which is called after a test suite is runned "
  " It is untested, so we return a random color"
  ^ Color black
]

{ #category : 'accessing' }
DTMockPluginResult >> buildAnotherTreeForUI [
	<pragmaForTest: 'for other tests' order: 2>
	^ DTTreeNode new
		subResults: {};
		yourself
]

{ #category : 'accessing' }
DTMockPluginResult >> buildTreeForUI [
	<pragmaForTest: 'for tests' order: 1>
	^ DTTreeNode new
		subResults: {};
		yourself
]
