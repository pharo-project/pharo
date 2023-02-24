Class {
	#name : #ZnOptionsTest,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #options }
ZnOptionsTest class >> bar [
	"Test option bar"

	<znOption>

	^ 2
]

{ #category : #options }
ZnOptionsTest class >> foo [
	"Test option foo"

	<znOption>

	^ 1
]

{ #category : #tests }
ZnOptionsTest >> testAllBindings [
	| options |
	options := (ZnOptions onClass: self class) clone.
	self assert: options allBindings equals: { #foo -> 1. #bar -> 2 } asDictionary.
	options at: #foo put: -1.
	self assert: options allBindings equals: { #foo -> -1. #bar -> 2 } asDictionary.
	options := options clone.
	options at: #bar put: -2.
	self assert: options allBindings equals: { #foo -> -1. #bar -> -2 } asDictionary.
]

{ #category : #tests }
ZnOptionsTest >> testClone [
	| options |
	options := (ZnOptions onClass: self class) clone.
	self assert: (options at: #foo) equals: 1.
	self assert: (options at: #bar) equals: 2.
	options at: #foo put: -1.
	self assert: (options at: #foo) equals: -1.
	self should: [ options at: #xyz ] raise: KeyNotFound.
	self should: [ options at: #xyz put: -1 ] raise: KeyNotFound.
	options reset.
	self assert: (options at: #foo) equals: 1.
]

{ #category : #tests }
ZnOptionsTest >> testCreate [
	| options |
	options := ZnOptions onClass: self class.
	self assert: (options at: #foo) equals: 1.
	self assert: (options at: #bar) equals: 2.
	self assert: (options includesKey: #foo).
	self should: [ options at: #foo put: 0 ] raise: ModificationForbidden.
	self should: [ options at: #xyz ] raise: KeyNotFound.
	self should: [ options at: #xyz put: -1 ] raise: ModificationForbidden.
	self should: [ options writable: true ] raise: ModificationForbidden
]

{ #category : #tests }
ZnOptionsTest >> testCurrent [
	| options |
	self should: [ ZnCurrentOptions at: #foo ] raise: KeyNotFound.
	options := (ZnOptions onClass: self class) clone.
	self assert: (options at: #foo) equals: 1.
	options during: [
		self assert: (ZnCurrentOptions at: #foo) equals: 1 ].
	options clone
		at: #foo put: -1;
		during: [
			self assert: (ZnCurrentOptions at: #foo) equals: -1 ].
	self assert: (options at: #foo) equals: 1
]

{ #category : #tests }
ZnOptionsTest >> testCurrentConditionally [
	| options |
	self should: [ ZnCurrentOptions at: #foo ] raise: KeyNotFound.
	options := (ZnOptions onClass: self class) clone.
	self assert: (options at: #foo) equals: 1.
	options conditionallyDuring: [
		self assert: (ZnCurrentOptions at: #foo) equals: 1 ].
	options clone
		at: #foo put: -1;
		conditionallyDuring: [
			self assert: (ZnCurrentOptions at: #foo) equals: -1 ].
	options clone
		at: #foo put: -1;
		conditionallyDuring: [
			options clone
				at: #foo put: 0;
				conditionallyDuring: [
					self assert: (ZnCurrentOptions at: #foo) equals: -1 ] ].
	self assert: (options at: #foo) equals: 1.
]

{ #category : #tests }
ZnOptionsTest >> testPublicAPI [
	| localOptions originalValue |
	localOptions := ZnOptions globalDefault clone.
	originalValue := ZnOptions globalDefault at: #signalProgress.
	"Access an option, anywhere"
	self assert: (ZnCurrentOptions at: #signalProgress) equals: originalValue.
	"Set a global writable option, visible to anyone"
	ZnOptions globalDefault at: #signalProgress put: originalValue not.
	self assert: (ZnCurrentOptions at: #signalProgress) equals: originalValue not.
	ZnOptions globalDefault at: #signalProgress put: originalValue.
	"Access an option, using local option, unless set dynamically"
	localOptions conditionallyDuring: [
		self assert: (ZnCurrentOptions at: #signalProgress) equals: originalValue ].
	"Access an option, using local option, unless set dynamically, when set locally"
	localOptions at: #signalProgress put: originalValue not.
	localOptions conditionallyDuring: [
		self assert: (ZnCurrentOptions at: #signalProgress) equals: originalValue not ].
	"Access an option, using local option, unless set dynamically, when set locally,
	but again overwritten dynamically"
	localOptions at: #signalProgress put: originalValue not.
	localOptions clone
		at: #signalProgress put: originalValue;
		during: [
			localOptions conditionallyDuring: [
				self assert: (ZnCurrentOptions at: #signalProgress) equals: originalValue ] ]
]
