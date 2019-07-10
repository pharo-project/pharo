"
I am tree model used by the Spec infrastructure to provide a ""pluggable"" api since we do not have multiple inheritance in Pharo
"
Class {
	#name : #SpTreeModel,
	#superclass : #MorphTreeModel,
	#instVars : [
		'model',
		'rootNodes'
	],
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #'instance creation' }
SpTreeModel class >> model: model [

	^ self new
		model: model;
		yourself
]

{ #category : #'drag and drop - private' }
SpTreeModel >> acceptDroppingMorph: draggedMorph event: event inMorph: source [

	^ self model acceptDroppingMorph: draggedMorph event: event inMorph: source
]

{ #category : #overrided }
SpTreeModel >> doubleClick [

	self model doubleClick
]

{ #category : #'drag and drop - private' }
SpTreeModel >> dragPassengerFor: item inMorph: dragSource [ 
	
	^ self model dragPassengerFor: item inMorph: dragSource
]

{ #category : #'drag and drop - private' }
SpTreeModel >> dragPassengersFor: item inMorph: dragSource [
	
	^ self model dragPassengersFor: item inMorph: dragSource
]

{ #category : #accessing }
SpTreeModel >> menu: menu shifted: b [

	^ self model menu: menu shifted: b
]

{ #category : #accessing }
SpTreeModel >> model [
	^ model
]

{ #category : #accessing }
SpTreeModel >> model: anObject [
	model := anObject
]

{ #category : #private }
SpTreeModel >> resetRootNodes [

	rootNodes := nil
]

{ #category : #accessing }
SpTreeModel >> rootItems [

	^ self model roots
]

{ #category : #accessing }
SpTreeModel >> rootNodeFromItem: item [

	^ self model rootNodeFromItem: item
]

{ #category : #accessing }
SpTreeModel >> rootNodes [

	^ rootNodes ifNil: [ rootNodes := super rootNodes ]
]

{ #category : #'drag and drop - private' }
SpTreeModel >> transferFor: passenger from:  source [

	^ self model transferFor: passenger from: source
]

{ #category : #'drag and drop - private' }
SpTreeModel >> wantsDroppedMorph: draggedMorph event: anEvent inMorph: source [

	^ self model wantsDroppedMorph: draggedMorph event: anEvent inMorph: source
]
