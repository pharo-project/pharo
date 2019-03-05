"
A SpecLayoutAdd represents the action to add a subwidget into a spec layout
"
Class {
	#name : #SpecLayoutAdd,
	#superclass : #SpecLayoutSend,
	#instVars : [
		'layoutFrame',
		'subwidget',
		'subwidgetArguments'
	],
	#category : #'Spec-Layout-Actions'
}

{ #category : #'instance creation' }
SpecLayoutAdd class >> subwidget: subwidget layoutFrame: aLayoutFrame [

	^ self new
		subwidget: subwidget 
		layoutFrame: aLayoutFrame asSpecLayoutFrame; 
		yourself
]

{ #category : #'instance creation' }
SpecLayoutAdd >> arguments [
	"We store the result because it change the state of the layouts and should be called only one time. I do not like this implementation but we'll clean later if we have time."
	subwidgetArguments ifNil: [ subwidgetArguments := self subwidgetArguments ].
	^ {{subwidgetArguments. #layout:. layoutFrame generateSpec}}
]

{ #category : #protocol }
SpecLayoutAdd >> bottomFraction [

	^ layoutFrame bottomFraction
]

{ #category : #protocol }
SpecLayoutAdd >> bottomFraction: aNumber [

	layoutFrame bottomFraction: aNumber 
]

{ #category : #protocol }
SpecLayoutAdd >> bottomOffset [

	^ layoutFrame bottomOffset 
]

{ #category : #protocol }
SpecLayoutAdd >> bottomOffset: aNumber [

	layoutFrame bottomOffset: aNumber
]

{ #category : #accessing }
SpecLayoutAdd >> fractions [
	^ layoutFrame fractionRectangle
]

{ #category : #initialization }
SpecLayoutAdd >> initialize [

	super initialize.
	self selector: #add:.
	layoutFrame := SpecLayoutFrame identity
]

{ #category : #accessing }
SpecLayoutAdd >> layoutFrame [

	^ layoutFrame
]

{ #category : #accessing }
SpecLayoutAdd >> layoutFrame: newFrame [

	layoutFrame := newFrame asSpecLayoutFrame
]

{ #category : #protocol }
SpecLayoutAdd >> leftFraction [

	^ layoutFrame leftFraction 
]

{ #category : #protocol }
SpecLayoutAdd >> leftFraction: aNumber [

	layoutFrame leftFraction: aNumber

]

{ #category : #protocol }
SpecLayoutAdd >> leftOffset [

	^ layoutFrame leftOffset
]

{ #category : #protocol }
SpecLayoutAdd >> leftOffset: aNumber [

	^ layoutFrame leftOffset: aNumber
]

{ #category : #accessing }
SpecLayoutAdd >> offsets [
	"to deprecate 
	"
	self flag: #fix.
	^ layoutFrame leftOffset  @ layoutFrame topOffset corner:  layoutFrame rightOffset  @ layoutFrame bottomOffset 
]

{ #category : #protocol }
SpecLayoutAdd >> rightFraction [

	^ layoutFrame rightFraction
]

{ #category : #protocol }
SpecLayoutAdd >> rightFraction: aNumber [

	^ layoutFrame rightFraction: aNumber
]

{ #category : #protocol }
SpecLayoutAdd >> rightOffset [

	^ layoutFrame rightOffset
]

{ #category : #protocol }
SpecLayoutAdd >> rightOffset: aNumber [

	layoutFrame rightOffset: aNumber
]

{ #category : #accessing }
SpecLayoutAdd >> subwidget [
	^ subwidget
]

{ #category : #accessing }
SpecLayoutAdd >> subwidget: anObject [
	subwidget := anObject
]

{ #category : #'instance creation' }
SpecLayoutAdd >> subwidget: aSpec layoutFrame: aLayoutFrame [
	subwidget := aSpec isSymbol
		ifTrue: [ {#presenter.
			aSpec} ]
		ifFalse: [ aSpec isCollection
				ifTrue: [ aSpec isEmpty
						ifTrue: [ "should raise an error" ]
						ifFalse: [ self flag: #todo.	"In one version of Pharo the #model should disaper. It's just here for retro compatibility"
							(#(#model #presenter) anySatisfy: [ :symbol | aSpec first = symbol ])
								ifTrue: [ aSpec ]
								ifFalse: [ {#presenter} , aSpec ] ] ]
				ifFalse: [ "Not a symbol or a collection. We assume it's an object to add directly (like a morph by example)" aSpec ] ].
	layoutFrame := aLayoutFrame
]

{ #category : #'instance creation' }
SpecLayoutAdd >> subwidgetArguments [
	^ subwidget asArray
]

{ #category : #protocol }
SpecLayoutAdd >> topFraction [

	^ layoutFrame topFraction
]

{ #category : #protocol }
SpecLayoutAdd >> topFraction: aNumber [

	layoutFrame topFraction: aNumber
]

{ #category : #protocol }
SpecLayoutAdd >> topOffset [

	^ layoutFrame topOffset
]

{ #category : #protocol }
SpecLayoutAdd >> topOffset: aNumber [

	layoutFrame topOffset: aNumber
]
