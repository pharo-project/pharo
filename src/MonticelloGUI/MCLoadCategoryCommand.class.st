Class {
	#name : 'MCLoadCategoryCommand',
	#superclass : 'MCSnapshotBrowserCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'default' }
MCLoadCategoryCommand class >> defaultName [

	^ 'Load category'
]

{ #category : 'executing' }
MCLoadCategoryCommand >> categorySelection [

	^ context categorySelection
]

{ #category : 'executing' }
MCLoadCategoryCommand >> execute [
	"Load the entire selected category"

	self categorySelection ifNil: [ ^ self ].
	self methodsForSelectedClassCategory do: [ :m | m load ].
]

{ #category : 'executing' }
MCLoadCategoryCommand >> methodsForSelectedClassCategory [

	^ context methodsForSelectedClassCategory 
]
