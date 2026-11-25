"
Morph with an inset border by default and theme access.
"
Class {
	#name : 'ComposableMorph',
	#superclass : 'ModelMorph',
	#traits : 'TEasilyThemed - {#theme} + (TMorphicWidget - {#defaultBorderWidth})',
	#classTraits : 'TEasilyThemed classTrait + TMorphicWidget classTrait',
	#category : 'Morphic-Widgets-Basic',
	#package : 'Morphic-Widgets',
	#tag : 'Basic'
}

{ #category : 'initialization' }
ComposableMorph >> defaultBorderColor [
	"Answer the default border color/fill style for the receiver"

	^#inset
]

{ #category : 'accessing' }
ComposableMorph >> defaultTitle [
	"Answer the default title label for the receiver."

	^'Composite' translated
]

{ #category : 'instance creation' }
ComposableMorph >> newWindow [
	"Answer a new window with the receiver as model,
	except when the receiver is a morph."

	|w|
	w := StandardWindow new
		model: (self isMorph ifFalse: [self]);
		title: self defaultTitle;
		addMorph: self
		fullFrame: LayoutFrame identity;
		yourself.
	self borderWidth: 0.
	^w
]
