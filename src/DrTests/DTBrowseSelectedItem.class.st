"
I browse the item selected in middle list of DrTests.
"
Class {
	#name : #DTBrowseSelectedItem,
	#superclass : #DTMiddleListCommand,
	#category : #'DrTests-Commands'
}

{ #category : #testing }
DTBrowseSelectedItem >> canBeExecuted [
	^ self selectedItems size = 1
]

{ #category : #executing }
DTBrowseSelectedItem >> execute [
	self drTest browseSelectedItem
]

{ #category : #initialization }
DTBrowseSelectedItem >> initialize [
	super initialize.
	self name: 'Browse'
]
