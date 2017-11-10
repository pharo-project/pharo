"
Tree item for PluggableTreeMorph.
"
Class {
	#name : #PluggableTreeItemNode,
	#superclass : #ListItemWrapper,
	#category : #Morphic-Widgets-Pluggable
}

{ #category : #accessing }
PluggableTreeItemNode >> acceptDroppingObject: anotherItem [
	^model dropNode: anotherItem on: self
]

{ #category : #accessing }
PluggableTreeItemNode >> asString [
	^model printNode: self
]

{ #category : #accessing }
PluggableTreeItemNode >> balloonText [
	^model balloonTextForNode: self
]

{ #category : #accessing }
PluggableTreeItemNode >> canBeDragged [
	^model isDraggableNode: self
]

{ #category : #accessing }
PluggableTreeItemNode >> contents [
	^model contentsOfNode: self
]

{ #category : #accessing }
PluggableTreeItemNode >> hasContents [
	^model hasNodeContents: self
]

{ #category : #accessing }
PluggableTreeItemNode >> icon [
	^model iconOfNode: self
]

{ #category : #accessing }
PluggableTreeItemNode >> wantsDroppedObject: anotherItem [
	^model wantsDroppedNode: anotherItem on: self
]
