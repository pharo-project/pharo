"
I mock the state of StDebuggerUsingSpecSelector to test variations in its state.
"
Class {
	#name : #StDebuggerUsingSpecSelectorMock,
	#superclass : #StDebuggerSpecSelector,
	#instVars : [
		'withDebuggerExtensions',
		'withActivatedExtensions'
	],
	#category : #'NewTools-Debugger-Tests-Utils'
}

{ #category : #initialization }
StDebuggerUsingSpecSelectorMock class >> initializeWithExtensionsAllActivated [
	^self new initializeWithExtensionsAllActivated
]

{ #category : #initialization }
StDebuggerUsingSpecSelectorMock class >> initializeWithExtensionsNoneActivated [
	^self new initializeWithExtensionsNoneActivated
]

{ #category : #initialization }
StDebuggerUsingSpecSelectorMock class >> initializeWithoutExtensionsAllActivated [
	^self new initializeWithoutExtensionsAllActivated
]

{ #category : #initialization }
StDebuggerUsingSpecSelectorMock class >> initializeWithoutExtensionsNoneActivated [
	^self new initializeWithoutExtensionsNoneActivated
]

{ #category : #accessing }
StDebuggerUsingSpecSelectorMock >> activateDebuggerExtensions [
	^withDebuggerExtensions 
]

{ #category : #accessing }
StDebuggerUsingSpecSelectorMock >> availableSpecSelectors [
	^ StDebugger availableSpecSelectors
		add: self nonDefaultSelector;
		yourself
]

{ #category : #specs }
StDebuggerUsingSpecSelectorMock >> columnInspectorsSpec [
]

{ #category : #accessing }
StDebuggerUsingSpecSelectorMock >> debuggerClass [ 	
	^self
]

{ #category : #accessing }
StDebuggerUsingSpecSelectorMock >> hasAnyActivatedExtension [
	^ withActivatedExtensions
]

{ #category : #initialization }
StDebuggerUsingSpecSelectorMock >> initializeWithExtensionsAllActivated [
	withDebuggerExtensions := true.
	withActivatedExtensions := true
]

{ #category : #initialization }
StDebuggerUsingSpecSelectorMock >> initializeWithExtensionsNoneActivated [
	withDebuggerExtensions := true.
	withActivatedExtensions := false
]

{ #category : #initialization }
StDebuggerUsingSpecSelectorMock >> initializeWithoutExtensionsAllActivated [
	withDebuggerExtensions := false.
	withActivatedExtensions := true
]

{ #category : #initialization }
StDebuggerUsingSpecSelectorMock >> initializeWithoutExtensionsNoneActivated [
	withDebuggerExtensions := false.
	withActivatedExtensions := false
]

{ #category : #accessing }
StDebuggerUsingSpecSelectorMock >> nonDefaultSelector [
	^#nonDefaultSelector
]
