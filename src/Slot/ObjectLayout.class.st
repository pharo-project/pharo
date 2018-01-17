"
I am the superclass of standard layouts for Objects.
"
Class {
	#name : #ObjectLayout,
	#superclass : #AbstractLayout,
	#instVars : [
		'compactClassIndex'
	],
	#category : #'Slot-Layout'
}

{ #category : #'instance creation' }
ObjectLayout class >> extending: superLayout scope: aScope host: aClass [
	self subclassResponsibility
]

{ #category : #accessing }
ObjectLayout class >> layoutForType: typeSymbol [
	typeSymbol = #compiledMethod 
		ifTrue: [ ^ CompiledMethodLayout ].
	typeSymbol = #bytes 
		ifTrue: [ ^ ByteLayout ].
	typeSymbol = #words 
		ifTrue: [ ^ WordLayout ].
	typeSymbol = #weak 
		ifTrue: [ ^ WeakLayout ].
	typeSymbol = #variable 
		ifTrue: [ ^ VariableLayout ].
	typeSymbol = #normal 
		ifTrue: [ ^ FixedLayout ].
	typeSymbol = #immediate 
		ifTrue: [ ^ ImmediateLayout ].
	typeSymbol = #ephemeron
		ifTrue: [ ^ EphemeronLayout ].
	"hack to support user defined layouts"
	(Smalltalk hasClassNamed: typeSymbol) ifTrue: [ ^Smalltalk globals at: typeSymbol ].
	Error signal: 'Invalid layout type: ', typeSymbol asString.
]

{ #category : #diff }
ObjectLayout >> computeChangesFrom: other in: modification [
	other hasSlots ifTrue: [
		modification removals: other allSlots asArray ]
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
ObjectLayout >> initialize [
	compactClassIndex := 0
]

{ #category : #'initialize-release' }
ObjectLayout >> initializeInstance: anInstance [
	self subclassResponsibility
]

{ #category : #format }
ObjectLayout >> instanceSpecification [
	self subclassResponsibility
]

{ #category : #reshaping }
ObjectLayout >> reshapeFrom: oldScope to: newParentLayout [
	"Answer the reshaped version of me for a new parent layout."

	| newScope |
	newScope := self slotScope 
		rebase: oldScope 
		to: newParentLayout slotScope.
	^ newParentLayout extendAgain: self with: newScope.
]

{ #category : #reshaping }
ObjectLayout >> reshapeTo: aModification [
	^ self class 
		extending: aModification classLayout
		scope: LayoutEmptyScope instance
		host: self host
]
