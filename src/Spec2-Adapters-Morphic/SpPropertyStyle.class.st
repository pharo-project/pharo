"
# Style properties (`SpPropertyStyle`)

Style properties define different kind of properties a morphic component can have. There are several types of properties, defined as: 

- `SpContainerStyle`
- `SpDrawStyle`
- `SpFontStyle`
- `SpGeometryStyle`
"
Class {
	#name : #SpPropertyStyle,
	#superclass : #SpStyle,
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #copying }
SpPropertyStyle >> , other [

	^ self mergeWith: other
]

{ #category : #operations }
SpPropertyStyle >> applyTo: aMorph [
	
]

{ #category : #'instance creation' }
SpPropertyStyle >> fromSton: stonReader [

	"I will use accessors because I want to be able to add convenience methods"
	stonReader parseMapDo: [ :message :value |
		self 
			perform: message asMutator 
			with: (value ifNotNil: [ value asStyleVariable ]) ]
]

{ #category : #testing }
SpPropertyStyle >> isGeometry [

	^ false
]

{ #category : #testing }
SpPropertyStyle >> isStyleProperty [

	^ true
]

{ #category : #operations }
SpPropertyStyle >> mergeWith: otherProperty [
	| merged |

	self species = otherProperty species ifFalse: [ 
		self error: 'Trying to merge two different properties' ].
 
	merged := self species new.
	self class allSlots do: [ :eachSlot | 
		"other property will have precedence over this one"
		merged 
			writeSlot: eachSlot 
			value: ((otherProperty readSlot: eachSlot) ifNil: [ self readSlot: eachSlot ]) ].
		
	^ merged
]

{ #category : #private }
SpPropertyStyle >> toColor: aColorOrSymbol [

	"Try if color"
	aColorOrSymbol isSymbol ifFalse: [ ^ aColorOrSymbol ].
	"Try by name"
	(Color named: aColorOrSymbol) ifNotNil: [ :aColor | ^ aColor ]. 
	"Try by HEX (CSS style)"				
	^ Color fromHexString: aColorOrSymbol
]

{ #category : #private }
SpPropertyStyle >> toVariable: anObject [

	^ anObject ifNotNil: [ anObject asStyleVariable ].
	
]
