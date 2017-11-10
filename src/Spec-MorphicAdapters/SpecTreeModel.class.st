"
I am tree model used by the Spec infrastructure to provide a ""pluggable"" api since we do not have multiple inheritance in Pharo
"
Class {
	#name : #SpecTreeModel,
	#superclass : #MorphTreeModel,
	#instVars : [
		'model',
		'rootNodes'
	],
	#category : #Spec-MorphicAdapters-Support
}

{ #category : #'instance creation' }
SpecTreeModel class >> model: model [

	^ self new
		model: model;
		yourself
]

{ #category : #'drag and drop - private' }
SpecTreeModel >> acceptDroppingMorph: draggedMorph event: event inMorph: source [

	^ self model acceptDroppingMorph: draggedMorph event: event inMorph: source
]

{ #category : #overrided }
SpecTreeModel >> doubleClick [

	self model doubleClick
]

{ #category : #'drag and drop - private' }
SpecTreeModel >> dragPassengerFor: item inMorph: dragSource [ 
	
	^ self model dragPassengerFor: item inMorph: dragSource
]

{ #category : #'drag and drop - private' }
SpecTreeModel >> dragPassengersFor: item inMorph: dragSource [
	
	^ self model dragPassengersFor: item inMorph: dragSource
]

{ #category : #accessing }
SpecTreeModel >> menu: menu shifted: b [

	^ self model menu: menu shifted: b
]

{ #category : #accessing }
SpecTreeModel >> model [
	^ model
]

{ #category : #accessing }
SpecTreeModel >> model: anObject [
	model := anObject
]

{ #category : #private }
SpecTreeModel >> resetRootNodes [

	rootNodes := nil
]

{ #category : #accessing }
SpecTreeModel >> rootItems [

	^ self model roots
]

{ #category : #accessing }
SpecTreeModel >> rootNodeFromItem: item [

	^ self model rootNodeFromItem: item
]

{ #category : #accessing }
SpecTreeModel >> rootNodes [

	^ rootNodes ifNil: [ rootNodes := super rootNodes ]
]

{ #category : #'drag and drop - private' }
SpecTreeModel >> transferFor: passenger from:  source [

	^ self model transferFor: passenger from: source
]

{ #category : #'drag and drop - private' }
SpecTreeModel >> wantsDroppedMorph: draggedMorph event: anEvent inMorph: source [

	^ self model wantsDroppedMorph: draggedMorph event: anEvent inMorph: source
]
