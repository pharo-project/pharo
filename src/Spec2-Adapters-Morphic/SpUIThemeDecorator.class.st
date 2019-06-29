"
I decorate a UITheme allowing to override desired properties.

See #example on class side.
"
Class {
	#name : #SpUIThemeDecorator,
	#superclass : #Object,
	#instVars : [
		'theme',
		'themeOverrideDict'
	],
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #examples }
SpUIThemeDecorator class >> exampleDecoratorToMakePluggableButtonRed [
	| normalColorBlock clickedColorBlock |
	normalColorBlock := [ :aButton |
							(aButton valueOfProperty: #noFill ifAbsent: [false]) 
								ifTrue: [ SolidFillStyle color: Color transparent ]
								ifFalse: [ SolidFillStyle color: Color red ] ].
	clickedColorBlock := [ :aButton | SolidFillStyle color: Color red muchDarker ].
	^ (SpUIThemeDecorator theme: Smalltalk ui theme)
			property: #buttonNormalFillStyleFor: returnsValueOf: normalColorBlock;
			property: #buttonMouseOverFillStyleFor: returnsValueOf: normalColorBlock;
			property: #buttonPressedFillStyleFor: returnsValueOf: clickedColorBlock;
			yourself
]

{ #category : #'instance creation' }
SpUIThemeDecorator class >> theme: aUITheme [
	^ self new
		theme: aUITheme;
		yourself
]

{ #category : #'reflective operations' }
SpUIThemeDecorator >> doesNotUnderstand: aMessage [
	(self hasProperty: aMessage selector)
		ifTrue: [ |objOrBlock|
			objOrBlock := self objectAtProperty: aMessage selector.
			^ objOrBlock isBlock
				ifTrue: [ objOrBlock valueWithPossibleArgs: aMessage arguments ]
				ifFalse: [ objOrBlock value ] ].
	
	self theme
		ifNotNil: [ :t | ^ t perform: aMessage selector withArguments: aMessage arguments ].
	
	^ super doesNotUnderstand: aMessage
]

{ #category : #testing }
SpUIThemeDecorator >> hasProperty: aSymbol [
	^ themeOverrideDict includesKey: aSymbol
]

{ #category : #initialization }
SpUIThemeDecorator >> initialize [
	super initialize.
	themeOverrideDict := Dictionary new
]

{ #category : #accessing }
SpUIThemeDecorator >> objectAtProperty: aSymbol [
	^ themeOverrideDict at: aSymbol
]

{ #category : #accessing }
SpUIThemeDecorator >> property: aSymbol returnsValueOf: anObject [
	themeOverrideDict
		at: aSymbol put: anObject
]

{ #category : #accessing }
SpUIThemeDecorator >> theme [
	^ theme
]

{ #category : #accessing }
SpUIThemeDecorator >> theme: anObject [
	theme := anObject
]
