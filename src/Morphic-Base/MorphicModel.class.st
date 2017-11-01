"
MorphicModels are used to represent structures with state and behavior as well as graphical structure.  A morphicModel is usually the root of a morphic tree depicting its appearance.  The tree is constructed concretely by adding its consituent morphs to a world.

When a part is named in a world, it is given a new slot in the model.  When a part is sensitized, it is named, and a set of mouse-driven methods is also generated in the model.  These may be edited to induce particular behavior.  When a variable is added through the morphic world, it is given a slot in the model, along with a set of access methods.

In addition for public variables (and this is the default for now), methods are generated and called in any outer model in which this model gets embedded, thus propagating variable changes outward.
"
Class {
	#name : #MorphicModel,
	#superclass : #BorderedMorph,
	#instVars : [
		'model'
	],
	#classVars : [
		'KeyboardFocusOnMouseDown',
		'MouseOverForKeyboardFocus'
	],
	#category : 'Morphic-Base-Kernel'
}

{ #category : #settings }
MorphicModel class >> keyboardFocusOnMouseDown [
	^ KeyboardFocusOnMouseDown ifNil: [KeyboardFocusOnMouseDown := true]
]

{ #category : #settings }
MorphicModel class >> keyboardFocusOnMouseDown: aBoolean [
	KeyboardFocusOnMouseDown := aBoolean
]

{ #category : #settings }
MorphicModel class >> mouseOverForKeyboardFocus [
	^ MouseOverForKeyboardFocus ifNil: [MouseOverForKeyboardFocus := false]
]

{ #category : #settings }
MorphicModel class >> mouseOverForKeyboardFocus: aBoolean [
	MouseOverForKeyboardFocus := aBoolean
]

{ #category : #initialization }
MorphicModel >> defaultBorderColor [
	"answer the default border color/fill style for the receiver"
	^ Color yellow
]

{ #category : #initialization }
MorphicModel >> defaultBounds [
"answer the default bounds for the receiver"
	^ 0 @ 0 corner: 200 @ 100
]

{ #category : #initialization }
MorphicModel >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ Color transparent
]

{ #category : #menu }
MorphicModel >> hasYellowButtonMenu [
	"Answer true if I have any items at all for a context (yellow button) menu."
	^ self wantsYellowButtonMenu
			or: [model
					ifNil: [false]
					ifNotNil: [:aModel | aModel hasModelYellowButtonMenuItems]]
]

{ #category : #settings }
MorphicModel >> keyboardFocusOnMouseDown [
	^ self class keyboardFocusOnMouseDown
]

{ #category : #accessing }
MorphicModel >> model [ 
	^ model
]

{ #category : #initialization }
MorphicModel >> model: anObject [
	"Set my model and make me me a dependent of the given object."

	model ifNotNil: [model removeDependent: self].
	anObject ifNotNil: [anObject addDependent: self].
	model := anObject.

]

{ #category : #settings }
MorphicModel >> mouseOverForKeyboardFocus [
	^ self class mouseOverForKeyboardFocus
]

{ #category : #caching }
MorphicModel >> releaseCachedState [
	"Release cached state of the receiver"

	(model ~~ self and: [model respondsTo: #releaseCachedState]) ifTrue:
		[model releaseCachedState].
	super releaseCachedState
]
