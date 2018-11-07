"
I decorate a UITheme allowing to override desired properties.

See #example on class side.
"
Class {
	#name : #UIThemeDecorator,
	#superclass : #Object,
	#instVars : [
		'theme',
		'themeOverrideDict'
	],
	#category : #'Spec-MorphicAdapters-Support'
}

{ #category : #examples }
UIThemeDecorator class >> exampleDecoratorToMakePluggableButtonRed [
	| normalColorBlock clickedColorBlock |
	normalColorBlock := [ :aButton |
							(aButton valueOfProperty: #noFill ifAbsent: [false]) 
								ifTrue: [ SolidFillStyle color: Color transparent ]
								ifFalse: [ SolidFillStyle color: Color red ] ].
	clickedColorBlock := [ :aButton | SolidFillStyle color: Color red muchDarker ].
	^ (UIThemeDecorator theme: Smalltalk ui theme)
			property: #buttonNormalFillStyleFor: returnsValueOf: normalColorBlock;
			property: #buttonMouseOverFillStyleFor: returnsValueOf: normalColorBlock;
			property: #buttonPressedFillStyleFor: returnsValueOf: clickedColorBlock;
			yourself
]

{ #category : #'instance creation' }
UIThemeDecorator class >> theme: aUITheme [
	^ self new
		theme: aUITheme;
		yourself
]

{ #category : #'reflective operations' }
UIThemeDecorator >> doesNotUnderstand: aMessage [
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
UIThemeDecorator >> hasProperty: aSymbol [
	^ themeOverrideDict includesKey: aSymbol
]

{ #category : #initialization }
UIThemeDecorator >> initialize [
	super initialize.
	themeOverrideDict := Dictionary new
]

{ #category : #accessing }
UIThemeDecorator >> objectAtProperty: aSymbol [
	^ themeOverrideDict at: aSymbol
]

{ #category : #accessing }
UIThemeDecorator >> property: aSymbol returnsValueOf: anObject [
	themeOverrideDict
		at: aSymbol put: anObject
]

{ #category : #accessing }
UIThemeDecorator >> theme [
	^ theme
]

{ #category : #accessing }
UIThemeDecorator >> theme: anObject [
	theme := anObject
]
