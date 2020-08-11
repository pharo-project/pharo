"
! Style properties (==SpStyleProperty==)

Style properties define different kind of properties a morphic component can have. There are several types of properties, defined as: 

- *class:SpStyleContainer*
- *class:SpStyleDraw*
- *class:SpStyleFont*
- *class:SpStyleGeometry*
"
Class {
	#name : #SpStyleProperty,
	#superclass : #SpStyle,
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #copying }
SpStyleProperty >> , other [

	^ self mergeWith: other
]

{ #category : #operations }
SpStyleProperty >> applyTo: aMorph [
	
]

{ #category : #'instance creation' }
SpStyleProperty >> fromSton: stonReader [

	"I will use accessors because I want to be able to add convenience methods"
	stonReader parseMapDo: [ :message :value |
		self 
			perform: message asMutator 
			with: value ]
]

{ #category : #testing }
SpStyleProperty >> isStyleProperty [

	^ true
]

{ #category : #operations }
SpStyleProperty >> mergeWith: otherProperty [
	| merged |

	self species = otherProperty species ifFalse: [ 
		self error: 'Trying to merge two different properties' ].
 
	merged := self species new.
	self class allSlots do: [ :eachSlot | 
		"other property will have precedence over this one"
		merged 
			writeSlot: eachSlot 
			value: ((otherProperty readSlot: eachSlot) ifNil:[ self readSlot: eachSlot ]) ].
		
	^ merged
]

{ #category : #private }
SpStyleProperty >> toColor: aColorOrSymbol [

	"Try if color"
	aColorOrSymbol isSymbol ifFalse: [ ^ aColorOrSymbol ].
	"Try by name"
	(Color named: aColorOrSymbol) ifNotNil: [ :aColor | ^ aColor ]. 
	"Try by HEX (CSS style)"				
	^ Color fromHexString: aColorOrSymbol
]
