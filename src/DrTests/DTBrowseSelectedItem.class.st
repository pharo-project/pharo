"
I browse the item selected in middle list of DrTests.
"
Class {
	#name : #DTBrowseSelectedItem,
	#superclass : #DTMiddleListCommand,
	#category : #'DrTests-Commands'
}

{ #category : #defaults }
DTBrowseSelectedItem class >> defaultName [ 
	"Return the default name of the command"
	
	^ 'Browse'
]

{ #category : #testing }
DTBrowseSelectedItem >> canBeExecuted [

	^ self selectedItems size = 1
]

{ #category : #executing }
DTBrowseSelectedItem >> execute [

	self drTest browseSelectedItem
]
