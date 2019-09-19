"
A SpecLayoutAdd represents the action to add a subwidget into a spec layout
"
Class {
	#name : #SpecLayoutAdd,
	#superclass : #SpecLayoutSend,
	#instVars : [
		'layoutFrame',
		'subwidget'
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

{ #category : #'instance creation' }
SpecLayoutAdd >> generateArguments [
	
	self arguments: {{subwidget .#layout:. layoutFrame generateSpec}}.
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
SpecLayoutAdd >> subwidget: aSpec fractions: frac offsets: off [

	subwidget := aSpec isSymbol
				ifTrue: [{#model. aSpec}]
				ifFalse: [
					aSpec isCollection
						ifTrue: [
							aSpec isEmpty 
								ifTrue: [ "should raise an error" ]
								ifFalse: [ aSpec first == #model 
											ifTrue: [ aSpec ]
											ifFalse: [ {#model}, aSpec ]]]
						ifFalse: [ 
							"Not a symbol or a collection. We assume it's an object to add directly (like a morph by example)"
							aSpec ]].
	self fractions: frac.
	self offsets: off.
	
	self generateArguments
]

{ #category : #'instance creation' }
SpecLayoutAdd >> subwidget: aSpec layoutFrame: aLayoutFrame [
	
	subwidget := aSpec isSymbol
				ifTrue: [{#model. aSpec}]
				ifFalse: [
					aSpec isCollection
						ifTrue: [
							aSpec isEmpty 
								ifTrue: [ "should raise an error" ]
								ifFalse: [ aSpec first == #model 
											ifTrue: [ aSpec ]
											ifFalse: [ {#model}, aSpec ]]]
						ifFalse: [ 
							"Not a symbol or a collection. We assume it's an object to add directly (like a morph by example)"
							aSpec ]].
	
	layoutFrame := aLayoutFrame.
	self generateArguments
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
