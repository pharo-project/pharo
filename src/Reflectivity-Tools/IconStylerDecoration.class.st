"
I am a value object describing a text decoration produced by an IconStyler.

I hold all the properties needed to render a styled segment (icon, color, label, etc.) without depending on any backend framework. Renderer classes (SpIconStylerRenderer or RubricIconStylerRenderer) convert me into the appropriate native decoration.
"
Class {
	#name : 'IconStylerDecoration',
	#superclass : 'Object',
	#instVars : [
		'name',
		'interval',
		'highlightColor',
		'icon',
		'iconBlock',
		'title',
		'isUnderline',
		'underlineColor'
	],
	#category : 'Reflectivity-Tools-Styler',
	#package : 'Reflectivity-Tools',
	#tag : 'Styler'
}

{ #category : 'accessing' }
IconStylerDecoration >> highlightColor [

	^ highlightColor
]

{ #category : 'accessing' }
IconStylerDecoration >> highlightColor: anObject [

	highlightColor := anObject
]

{ #category : 'accessing' }
IconStylerDecoration >> icon [

	^ icon
]

{ #category : 'accessing' }
IconStylerDecoration >> icon: anObject [

	icon := anObject
]

{ #category : 'accessing' }
IconStylerDecoration >> iconBlock [

	^ iconBlock
]

{ #category : 'accessing' }
IconStylerDecoration >> iconBlock: anObject [

	iconBlock := anObject
]

{ #category : 'accessing' }
IconStylerDecoration >> interval [

	^ interval
]

{ #category : 'accessing' }
IconStylerDecoration >> interval: anObject [

	interval := anObject
]

{ #category : 'testing' }
IconStylerDecoration >> isUnderline [

	^ isUnderline ifNil: [ false ]
]

{ #category : 'accessing' }
IconStylerDecoration >> name [

	^ name
]

{ #category : 'accessing' }
IconStylerDecoration >> name: anObject [

	name := anObject
]

{ #category : 'accessing' }
IconStylerDecoration >> title [

	^ title
]

{ #category : 'accessing' }
IconStylerDecoration >> title: anObject [

	title := anObject
]

{ #category : 'accessing' }
IconStylerDecoration >> underline: aBoolean [

	isUnderline := aBoolean
]

{ #category : 'accessing' }
IconStylerDecoration >> underlineColor [

	^ underlineColor ifNil: [ Color transparent ]
]

{ #category : 'accessing' }
IconStylerDecoration >> underlineColor: anObject [

	underlineColor := anObject
]
