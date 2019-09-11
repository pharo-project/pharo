Class {
	#name : #RubPluggableTextFieldWithFloatConverterExample,
	#superclass : #Object,
	#instVars : [
		'myFloat',
		'announcer'
	],
	#category : #'Rubric-Compatibility-toBeDeprecated'
}

{ #category : #examples }
RubPluggableTextFieldWithFloatConverterExample class >> exampleOpen [
	| st |
	st := self new newScrolledText.
 	st openInWorld

]

{ #category : #accessing }
RubPluggableTextFieldWithFloatConverterExample >> announcer [
	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : #menu }
RubPluggableTextFieldWithFloatConverterExample >> getMyMenu: aMenu [
	^ nil
]

{ #category : #accessing }
RubPluggableTextFieldWithFloatConverterExample >> myFloat [
	^ myFloat ifNil: [ myFloat := 0.0 ]

]

{ #category : #accessing }
RubPluggableTextFieldWithFloatConverterExample >> myFloat: aFloat [
	myFloat := aFloat.
	aFloat traceCr.
	^true
]

{ #category : #'user interface' }
RubPluggableTextFieldWithFloatConverterExample >> newScrolledText [
	| st |
	st := (RubPluggableTextFieldMorph new
		on: self
		text: #myFloat
		accept: #myFloat:
		readSelection: #readMySelection:
		menu: #getMyMenu:)
		withGhostText: 'Enter a float number here';
		yourself.
	st convertTo: Float.
	st width: 300.
	^ st
]

{ #category : #'user interface' }
RubPluggableTextFieldWithFloatConverterExample >> readMySelection: aSelection [
	^ nil
]
