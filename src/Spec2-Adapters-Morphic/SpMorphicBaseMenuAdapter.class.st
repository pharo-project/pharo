"
I am the adapter used to build a MenuMorph from a MenuPresenter
"
Class {
	#name : #SpMorphicBaseMenuAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #initialization }
SpMorphicBaseMenuAdapter >> addKeyBindingsTo: aMorph [
	"This does not has keybindings"
]

{ #category : #protocol }
SpMorphicBaseMenuAdapter >> icon [

	^ self model icon
]

{ #category : #protocol }
SpMorphicBaseMenuAdapter >> menuGroups [

	^ self model menuGroups reject: [ :e | e isEmpty ]
]

{ #category : #'spec protocol' }
SpMorphicBaseMenuAdapter >> openAt: aPoint [ 

	self widgetDo: [ :w | w invokeAt: (aPoint + (2@7)) in: self currentWorld allowKeyboard: true ]
]

{ #category : #protocol }
SpMorphicBaseMenuAdapter >> title [

	^ self model title
]
