"
I am the adapter used to build a MenuMorph from a MenuPresenter
"
Class {
	#name : #MorphicBaseMenuAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #protocol }
MorphicBaseMenuAdapter >> icon [

	^ self model icon
]

{ #category : #protocol }
MorphicBaseMenuAdapter >> menuGroups [

	^ self model menuGroups reject: [ :e | e isEmpty ]
]

{ #category : #'spec protocol' }
MorphicBaseMenuAdapter >> openAt: aPoint [ 

	self widgetDo: [ :w | w invokeAt: (aPoint + (2@7)) in: World allowKeyboard: true ]
]

{ #category : #protocol }
MorphicBaseMenuAdapter >> title [

	^ self model title
]
