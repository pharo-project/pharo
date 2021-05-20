Class {
	#name : #SpToggleMenuItemMorph,
	#superclass : #ToggleMenuItemMorph,
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #'drawing-private' }
SpToggleMenuItemMorph >> itemShortcut [

	"In Spec, this is always already normalised"
	^ (ToggleMenuItemShortcut owner: self keyText: '') 
		normalizedKeyText: self keyText;
		yourself
]
