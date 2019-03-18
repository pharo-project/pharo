Class {
	#name : #SlotLayoutExtensionTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-ClassBuilder'
}

{ #category : #helpers }
SlotLayoutExtensionTest >> should: superclassBlock extendWith: subclassBlock [

	| superclass |
	superclass := self make: superclassBlock.

	self make: [ :builder | 
		builder 
			superclass: superclass;
			name: self anotherClassName;
			in: subclassBlock ]
]

{ #category : #helpers }
SlotLayoutExtensionTest >> shouldnt: superclassBlock extendWith: subclassBlock [

	| superclass |
	superclass := self make: superclassBlock.

	self should: [
		self make: [ :builder | 
			builder 
				superclass: superclass;
				name: self anotherClassName;
				in: subclassBlock ] ]
		raise: IncompatibleLayoutConflict
]

{ #category : #'tests-valid extensions' }
SlotLayoutExtensionTest >> testByteCanExtendByte [

	self should: [ :builder | 
			builder layoutClass: ByteLayout ]
		extendWith: [ :builder | 
			builder layoutClass: ByteLayout ]
]

{ #category : #'tests-invalid extensions' }
SlotLayoutExtensionTest >> testByteCannotExtendPointerWithFields [

	self shouldnt: [ :builder | 
			builder 
				layoutClass: FixedLayout;
				slots: { #a }. ]
		extendWith: [ :builder | 
			builder layoutClass: ByteLayout ]
]

{ #category : #'tests-invalid extensions' }
SlotLayoutExtensionTest >> testByteCannotExtendWord [

	self shouldnt: [ :builder | 
			builder layoutClass: WordLayout ]
		extendWith: [ :builder | 
			builder layoutClass: ByteLayout ]
]

{ #category : #'tests-valid extensions' }
SlotLayoutExtensionTest >> testCompiledMethodCanBeExtended [

	self should: [ :builder | 
			builder layoutClass: CompiledMethodLayout ]
		extendWith: [ :builder | 
			builder layoutClass: CompiledMethodLayout ]
]

{ #category : #'tests-invalid extensions' }
SlotLayoutExtensionTest >> testInstanceVariableNamesMetaclassInterface [
	"Add instance variables using the Metaclass interface"
	
	aClass := self make: [ :builder |
		builder name: self aClassName ].
	aClass class instanceVariableNames: 'x'.

	self assert: aClass class instVarNames equals: #(x).	

	anotherClass := self make: [ :builder |
		builder 
			superclass: aClass;
			name: self anotherClassName ].
	anotherClass class instanceVariableNames: 'y'.
	
	self assert: anotherClass class instVarNames equals: #(y).	
	self assert: (anotherClass class allInstVarNames includesAll: #(x y)).
]

{ #category : #'tests-valid extensions' }
SlotLayoutExtensionTest >> testPointerCanExtendPointer [

	self should: [ :builder | 
			builder layoutClass: FixedLayout ]
		extendWith: [ :builder | 
			builder layoutClass: FixedLayout ]
]

{ #category : #'tests-valid extensions' }
SlotLayoutExtensionTest >> testPointerCanExtendVariable [

	self should: [ :builder | 
			builder layoutClass: VariableLayout ]
		extendWith: [ :builder | 
			builder layoutClass: FixedLayout ]
]

{ #category : #'tests-invalid extensions' }
SlotLayoutExtensionTest >> testPointerCannotExtendByte [

	self shouldnt: [ :builder | 
			builder layoutClass: ByteLayout ]
		extendWith: [ :builder | 
			builder layoutClass: FixedLayout ]
]

{ #category : #'tests-invalid extensions' }
SlotLayoutExtensionTest >> testPointerCannotExtendWord [

	self shouldnt: [ :builder | 
			builder layoutClass: WordLayout ]
		extendWith: [ :builder | 
			builder layoutClass: FixedLayout ]
]

{ #category : #'tests-valid extensions' }
SlotLayoutExtensionTest >> testPointerWithVariableExtensionIsNowVariable [
	"This test case reproduces a real bug."

	| superclass subclass |
	superclass := self make: [ :builder | 
		builder 
			superclass: Object;
			name: self aClassName ].

	subclass := self make: [ :builder | 
		builder 
			superclass: superclass;
			layoutClass: VariableLayout;
			name: self anotherClassName ].

	superclass := self make: [ :builder | 
		builder layoutClass: VariableLayout ].

	self assert: superclass isVariable.
	self assert: subclass isVariable.
]

{ #category : #'tests-valid extensions' }
SlotLayoutExtensionTest >> testVariableCanExtendPointer [

	self should: [ :builder | 
			builder layoutClass: FixedLayout ]
		extendWith: [ :builder | 
			builder layoutClass: VariableLayout ]
]

{ #category : #'tests-valid extensions' }
SlotLayoutExtensionTest >> testVariableCanExtendVariable [

	self should: [ :builder | 
			builder layoutClass: VariableLayout ]
		extendWith: [ :builder | 
			builder layoutClass: VariableLayout ]
]

{ #category : #'tests-invalid extensions' }
SlotLayoutExtensionTest >> testVariableCannotExtendByte [

	self shouldnt: [ :builder | 
			builder layoutClass: ByteLayout ]
		extendWith: [ :builder | 
			builder layoutClass: VariableLayout ]
]

{ #category : #'tests-invalid extensions' }
SlotLayoutExtensionTest >> testVariableCannotExtendWord [

	self shouldnt: [ :builder | 
			builder layoutClass: WordLayout ]
		extendWith: [ :builder | 
			builder layoutClass: VariableLayout ]
]

{ #category : #'tests-valid extensions' }
SlotLayoutExtensionTest >> testWordCanExtendWord [

	self should: [ :builder | 
			builder layoutClass: WordLayout ]
		extendWith: [ :builder | 
			builder layoutClass: WordLayout ]
]

{ #category : #'tests-invalid extensions' }
SlotLayoutExtensionTest >> testWordCannotExtendByte [

	self shouldnt: [ :builder | 
			builder layoutClass: ByteLayout ]
		extendWith: [ :builder | 
			builder layoutClass: WordLayout ]
]

{ #category : #'tests-invalid extensions' }
SlotLayoutExtensionTest >> testWordCannotExtendPointerWithFields [

	self shouldnt: [ :builder | 
			builder 
				layoutClass: FixedLayout;
				slots: { #a } ]
		extendWith: [ :builder | 
			builder layoutClass: WordLayout ]
]
