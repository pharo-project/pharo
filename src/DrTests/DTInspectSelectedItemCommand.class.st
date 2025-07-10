"
I browse the selected result in the results list of DrTests.
"
Class {
	#name : 'DTInspectSelectedItemCommand',
	#superclass : 'DTMiddleListCommand',
	#category : 'DrTests-Commands',
	#package : 'DrTests',
	#tag : 'Commands'
}

{ #category : 'default' }
DTInspectSelectedItemCommand class >> defaultName [

	^ 'Inspect'
]

{ #category : 'executing' }
DTInspectSelectedItemCommand >> execute [

	self context inspectSelectedItem
]
