"
A BitmapFillStyle fills using any kind of form.

Instance variables:
	form	<Form>	The form to be used as fill.
	tileFlag	<Boolean>	If true, then the form is repeatedly drawn to fill the area.
"
Class {
	#name : #BitmapFillStyle,
	#superclass : #OrientedFillStyle,
	#instVars : [
		'form',
		'tileFlag'
	],
	#category : 'Balloon-Fills'
}

{ #category : #'instance creation' }
BitmapFillStyle class >> form: aForm [
	^self new form: aForm
]

{ #category : #'instance creation' }
BitmapFillStyle class >> fromForm: aForm [
	| fs |
	fs := self form: aForm.
	fs origin: 0@0.
	fs direction: aForm width @ 0.
	fs normal: 0 @ aForm height.
	fs tileFlag: true.
	^fs
]

{ #category : #'instance creation' }
BitmapFillStyle class >> fromUser [
	| fill |
	fill := self form: Form fromUser.
	fill origin: 0@0.
	fill direction: fill form width @ 0.
	fill normal: 0 @ fill form height.
	fill tileFlag: true. "So that we can fill arbitrary objects"
	^fill
]

{ #category : #converting }
BitmapFillStyle >> asColor [
	^form colorAt: 0@0
]

{ #category : #accessing }
BitmapFillStyle >> direction [

	^direction ifNil: [direction :=( (normal y @ normal x negated) * form width / form height ) rounded]
]

{ #category : #accessing }
BitmapFillStyle >> form [
	^form
]

{ #category : #accessing }
BitmapFillStyle >> form: aForm [
	form := aForm
]

{ #category : #testing }
BitmapFillStyle >> isBitmapFill [
	^true
]

{ #category : #testing }
BitmapFillStyle >> isTiled [
	"Return true if the receiver should be repeated if the fill shape is larger than the form"
	^tileFlag == true
]

{ #category : #accessing }
BitmapFillStyle >> normal [
	^normal ifNil: [normal := ((direction y negated @ direction x) *  form height / form width ) rounded]
]

{ #category : #accessing }
BitmapFillStyle >> tileFlag [
	^tileFlag
]

{ #category : #accessing }
BitmapFillStyle >> tileFlag: aBoolean [
	tileFlag := aBoolean
]
