"
I am the superclass of standard layouts for Objects.
"
Class {
	#name : #ObjectLayout,
	#superclass : #AbstractLayout,
	#category : #'Kernel-Layout'
}

{ #category : #'instance creation' }
ObjectLayout class >> extending: superLayout scope: aScope host: aClass [
	self subclassResponsibility
]

{ #category : #testing }
ObjectLayout class >> isAbstract [
	^self == ObjectLayout
]

{ #category : #monticello }
ObjectLayout class >> layoutForType: typeSymbol [
	"used to get the layout for a Monticello internal layout symbol"
	^self allSubclasses 
		detect: [ :class | class mcTypeSymbol = typeSymbol ] 
		ifNone: [ Error signal: 'Invalid layout type: ', typeSymbol asString ]
]

{ #category : #monticello }
ObjectLayout class >> mcTypeSymbol [
	"return the symbol that Monticello uses to encode the layout"
	^self name asSymbol
]

{ #category : #description }
ObjectLayout class >> subclassDefiningSymbol [
	"As a fallback we just return a standard class creation symbol. This will be called for user 	
	defined Layouts, for old style class definitions that can not support user defined Layouts"
	^#subclass:
]

{ #category : #extending }
ObjectLayout >> extend [
	"Answer a default extension of me."

	^ self subclassResponsibility
]

{ #category : #extending }
ObjectLayout >> extend: aScope [
	IncompatibleLayoutConflict new
		layout: self;
		subType: #slot;
		signal
]

{ #category : #extending }
ObjectLayout >> extendByte [
	self hasFields ifTrue: [
		IncompatibleLayoutConflict new
			layout: self;
			subType: #byte;
			signal ].
	^ ByteLayout new
]

{ #category : #extending }
ObjectLayout >> extendCompiledMethod [
	self hasFields ifTrue: [
		IncompatibleLayoutConflict new
			layout: self;
			subType: #compiledMethod;
			signal ].
	^ CompiledMethodLayout new
]

{ #category : #extending }
ObjectLayout >> extendDoubleByte [
	self hasFields ifTrue: [
		IncompatibleLayoutConflict new
			layout: self;
			subType: #doubleByte;
			signal ].
	^ DoubleByteLayout new
]

{ #category : #extending }
ObjectLayout >> extendDoubleWord [
	self hasFields ifTrue: [
		IncompatibleLayoutConflict new
			layout: self;
			subType: #doubleWord;
			signal ].
	^ DoubleWordLayout new
]

{ #category : #extending }
ObjectLayout >> extendSmallInteger [
	self hasFields ifTrue: [
		IncompatibleLayoutConflict new
			layout: self;
			subType: #compiledMethod;
			signal ].
	^ ImmediateLayout new
]

{ #category : #extending }
ObjectLayout >> extendVariable: aScope [
	IncompatibleLayoutConflict new
		layout: self;
		subType: #variable;
		signal
]

{ #category : #extending }
ObjectLayout >> extendWord [
	self hasFields ifTrue: [
		IncompatibleLayoutConflict new
			layout: self;
			subType: #word;
			signal ].
	^ WordLayout new
]

{ #category : #format }
ObjectLayout >> format [
	"Answer an Integer that encodes this layout in the way that VM expects it."
	
	| fieldSize  format |
	fieldSize := self fieldSize.
	format := self instanceSpecification.
	format := (format bitShift: 16) + fieldSize.
	^ format
]

{ #category : #initialization }
ObjectLayout >> initializeInstance: anInstance [
	"do nothing by default, overriden by PointerLayout"
]

{ #category : #format }
ObjectLayout >> instanceSpecification [
	self subclassResponsibility
]

{ #category : #'testing - class hierarchy' }
ObjectLayout >> kindOfSubclass [
	^' ',self class subclassDefiningSymbol, ' '.
]

{ #category : #'testing - class hierarchy' }
ObjectLayout >> subclassDefiningSymbol [
	^self class subclassDefiningSymbol
]
