"
A SpecLayoutAdd represents the action to add a subwidget into a spec layout
"
Class {
	#name : #SpLayoutAdd,
	#superclass : #SpLayoutSend,
	#instVars : [
		'layoutFrame',
		'subwidget',
		'subwidgetArguments'
	],
	#category : #'Spec2-Layout-Deprecated'
}

{ #category : #'instance creation' }
SpLayoutAdd class >> subwidget: subwidget layoutFrame: aLayoutFrame [

	^ self new
		subwidget: subwidget 
		layoutFrame: aLayoutFrame asSpLayoutFrame; 
		yourself
]

{ #category : #'instance creation' }
SpLayoutAdd >> arguments [
	"We store the result because it change the state of the layouts and should be called only one time. I do not like this implementation but we'll clean later if we have time."

	subwidgetArguments ifNil: [ subwidgetArguments := self subwidgetArguments ].
	^ {{subwidgetArguments . #layout: . self generateSpec}}
]

{ #category : #protocol }
SpLayoutAdd >> bottomFraction [

	^ layoutFrame bottomFraction
]

{ #category : #protocol }
SpLayoutAdd >> bottomFraction: aNumber [

	layoutFrame bottomFraction: aNumber 
]

{ #category : #protocol }
SpLayoutAdd >> bottomOffset [

	^ layoutFrame bottomOffset 
]

{ #category : #protocol }
SpLayoutAdd >> bottomOffset: aNumber [

	layoutFrame bottomOffset: aNumber
]

{ #category : #accessing }
SpLayoutAdd >> fractions [
	^ layoutFrame fractionRectangle
]

{ #category : #'as yet unclassified' }
SpLayoutAdd >> generateSpec [
	| str spec |
	str := layoutFrame asArray readStream.	
	
	spec :=  OrderedCollection with: #SpLayoutFrame.
		
	#( leftFraction: topFraction: rightFraction: bottomFraction: leftOffset: topOffset: rightOffset: bottomOffset: ) do: [:sel |
		spec add: sel.
		str next ifNil: [ spec add: 0 ] ifNotNil: [:value | spec add: value ]].
	^ spec asArray
]

{ #category : #initialization }
SpLayoutAdd >> initialize [

	super initialize.
	self selector: #add:.
	layoutFrame := SpLayoutFrame identity
]

{ #category : #accessing }
SpLayoutAdd >> layoutFrame [

	^ layoutFrame
]

{ #category : #accessing }
SpLayoutAdd >> layoutFrame: newFrame [

	layoutFrame := newFrame asSpLayoutFrame
]

{ #category : #protocol }
SpLayoutAdd >> leftFraction [

	^ layoutFrame leftFraction 
]

{ #category : #protocol }
SpLayoutAdd >> leftFraction: aNumber [

	layoutFrame leftFraction: aNumber

]

{ #category : #protocol }
SpLayoutAdd >> leftOffset [

	^ layoutFrame leftOffset
]

{ #category : #protocol }
SpLayoutAdd >> leftOffset: aNumber [

	^ layoutFrame leftOffset: aNumber
]

{ #category : #accessing }
SpLayoutAdd >> offsets [
	"to deprecate 
	"
	self flag: #fix.
	^ layoutFrame leftOffset  @ layoutFrame topOffset corner:  layoutFrame rightOffset  @ layoutFrame bottomOffset 
]

{ #category : #protocol }
SpLayoutAdd >> rightFraction [

	^ layoutFrame rightFraction
]

{ #category : #protocol }
SpLayoutAdd >> rightFraction: aNumber [

	^ layoutFrame rightFraction: aNumber
]

{ #category : #protocol }
SpLayoutAdd >> rightOffset [

	^ layoutFrame rightOffset
]

{ #category : #protocol }
SpLayoutAdd >> rightOffset: aNumber [

	layoutFrame rightOffset: aNumber
]

{ #category : #accessing }
SpLayoutAdd >> subwidget [
	^ subwidget
]

{ #category : #accessing }
SpLayoutAdd >> subwidget: anObject [
	subwidget := anObject
]

{ #category : #'instance creation' }
SpLayoutAdd >> subwidget: aSpec layoutFrame: aLayoutFrame [
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
SpLayoutAdd >> subwidgetArguments [
	^ subwidget asArray
]

{ #category : #protocol }
SpLayoutAdd >> topFraction [

	^ layoutFrame topFraction
]

{ #category : #protocol }
SpLayoutAdd >> topFraction: aNumber [

	layoutFrame topFraction: aNumber
]

{ #category : #protocol }
SpLayoutAdd >> topOffset [

	^ layoutFrame topOffset
]

{ #category : #protocol }
SpLayoutAdd >> topOffset: aNumber [

	layoutFrame topOffset: aNumber
]
