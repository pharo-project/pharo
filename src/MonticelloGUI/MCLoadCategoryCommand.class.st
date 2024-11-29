Class {
	#name : 'MCLoadCategoryCommand',
	#superclass : 'MCSnapshotBrowserCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'default' }
MCLoadCategoryCommand class >> defaultName [

	^ 'Load category...'
]

{ #category : 'testing' }
MCLoadCategoryCommand >> canBeExecuted [ 

	^ self categorySelection notNil
]

{ #category : 'executing' }
MCLoadCategoryCommand >> categorySelection [

	^ context categorySelection
]

{ #category : 'executing' }
MCLoadCategoryCommand >> execute [
	"Load the entire selected category"

	self methodsForSelectedClassCategory 
		do: [ :m | m load ]
		displayingProgress: 'Loading definitions...'
]

{ #category : 'executing' }
MCLoadCategoryCommand >> methodsForSelectedClassCategory [

	^ context methodsForSelectedClassCategory 
]
