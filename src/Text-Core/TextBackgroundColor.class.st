"
I am a text attribute representing background color of a text. I am used as any other TextAttribute.


"
Class {
	#name : 'TextBackgroundColor',
	#superclass : 'TextAttribute',
	#instVars : [
		'color'
	],
	#category : 'Text-Core-Attributes',
	#package : 'Text-Core',
	#tag : 'Attributes'
}

{ #category : 'constants' }
TextBackgroundColor class >> black [
	^ self new color: Color black
]

{ #category : 'constants' }
TextBackgroundColor class >> blue [
	^ self new color: Color blue
]

{ #category : 'instance creation' }
TextBackgroundColor class >> color: aColor [
	^ self new color: aColor
]

{ #category : 'constants' }
TextBackgroundColor class >> cyan [
	^ self new color: Color cyan
]

{ #category : 'constants' }
TextBackgroundColor class >> gray [
	^ self new color: Color gray
]

{ #category : 'constants' }
TextBackgroundColor class >> green [
	^ self new color: Color green
]

{ #category : 'constants' }
TextBackgroundColor class >> magenta [
	^ self new color: Color magenta
]

{ #category : 'constants' }
TextBackgroundColor class >> red [
	^ self new color: Color red
]

{ #category : 'constants' }
TextBackgroundColor class >> white [
	^ self new color: Color white
]

{ #category : 'constants' }
TextBackgroundColor class >> yellow [
	^ self new color: Color yellow
]

{ #category : 'comparing' }
TextBackgroundColor >> = other [
	^ (other class == self class)
		and: [other color = color]
]

{ #category : 'converting' }
TextBackgroundColor >> asColor [

	^ color
]

{ #category : 'accessing' }
TextBackgroundColor >> color [
	^ color
]

{ #category : 'accessing' }
TextBackgroundColor >> color: aColor [
	color := aColor
]

{ #category : 'scanning' }
TextBackgroundColor >> dominates: other [
"Only one background color attribute on a same portion of text."
	^ other class == self class
]

{ #category : 'comparing' }
TextBackgroundColor >> hash [
	^ color hash
]
