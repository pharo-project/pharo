Class {
	#name : #FLEnvironmentProtectingWrapper,
	#superclass : #Object,
	#instVars : [
		#overlay,
		#environment,
		#sentinel
	],
	#category : #'Fuel-Tests-Core-Support-Environment'
}

{ #category : #'instance creation' }
FLEnvironmentProtectingWrapper class >> on: aDictionary [
	^ self basicNew
		initializeWithEnvironment: aDictionary;
		initialize;
		yourself
]

{ #category : #comparing }
FLEnvironmentProtectingWrapper >> = anObject [
	^ environment = anObject
]

{ #category : #'classes and traits' }
FLEnvironmentProtectingWrapper >> allClasses [
	^ self classNames collect: [ :key | self at: key ]
]

{ #category : #accessing }
FLEnvironmentProtectingWrapper >> associationAt: key [
	^ overlay
		associationAt: key
		ifAbsent: [
			environment associationAt: key ]
]

{ #category : #accessing }
FLEnvironmentProtectingWrapper >> associationAt: key ifAbsent: aBlock [
	^ overlay
		associationAt: key
		ifAbsent: [
			environment
				associationAt: key
				ifAbsent: aBlock ]
]

{ #category : #accessing }
FLEnvironmentProtectingWrapper >> at: key [
	^ overlay
		at: key asSymbol
		ifPresent: [ :value |
			self
				checkReturn: value
				forKey: key.
			value ]
		ifAbsent: [ environment at: key ]
]

{ #category : #accessing }
FLEnvironmentProtectingWrapper >> at: key ifAbsent: aBlock [
	| value |
	value := overlay
		at: key
		ifAbsent: [
			environment
				at: key
				ifAbsent: aBlock ].
	^ value == sentinel
		ifTrue: aBlock
		ifFalse: [ value ]
]

{ #category : #accessing }
FLEnvironmentProtectingWrapper >> at: key ifAbsentPut: aBlock [
	^ self
		at: key
		ifAbsent: [
			overlay
				at: key
				put: aBlock value ]
]

{ #category : #accessing }
FLEnvironmentProtectingWrapper >> at: key ifPresent: aBlock [
	^ overlay
		at: key
		ifPresent: [ :value |
			self
				checkReturn: value
				forKey: key.
			aBlock cull: value ]
		ifAbsent: [
			environment
				at: key
				ifPresent: aBlock ]
]

{ #category : #accessing }
FLEnvironmentProtectingWrapper >> at: key ifPresent: presentBlock ifAbsent: absentBlock [
	^ overlay
		at: key
		ifPresent: [ :value |
			value == sentinel
				ifTrue: absentBlock
				ifFalse: [ presentBlock cull: value ] ]
		ifAbsent: [
			environment
				at: key
				ifPresent: presentBlock
				ifAbsent: absentBlock ]
]

{ #category : #accessing }
FLEnvironmentProtectingWrapper >> at: key ifPresent: presentBlock ifAbsentPut: absentBlock [
	^ self
		at: key
		ifPresent: [ :value |
			value == sentinel
				ifTrue: [
					self
						at: key
						put: absentBlock value ]
				ifFalse: [ presentBlock cull: value ] ]
		ifAbsent: [
			self
				at: key
				put: absentBlock value ]
]

{ #category : #accessing }
FLEnvironmentProtectingWrapper >> at: key put: anObject [
	"This produces GlobalVariable instances"
	(overlay includesKey: key) ifFalse: [
		overlay
			declare: key
			from: Undeclared ].

	^ overlay
		at: key
		put: anObject
]

{ #category : #binding }
FLEnvironmentProtectingWrapper >> bindingOf: aString [
	^ overlay
		at: aString
		ifPresent: [ :value |
			value == sentinel
				ifTrue: [ ^ nil ]
				ifFalse: [ overlay bindingOf: aString ] ]
		ifAbsent: [ environment bindingOf: aString ]
]

{ #category : #accessing }
FLEnvironmentProtectingWrapper >> checkReturn: anObject forKey: aKey [
	anObject == sentinel ifTrue: [
		^ KeyNotFound signalFor: aKey ].
	^ anObject
]

{ #category : #'class and trait names' }
FLEnvironmentProtectingWrapper >> classNames [
	| class |
	class := self at: #Class.
	^ (overlay associations
		select: [ :assoc | assoc value isKindOf: class ]
		thenCollect: [ :assoc | assoc key ]) asSet
			addAll: environment classNames;
			yourself
]

{ #category : #'reflective operations' }
FLEnvironmentProtectingWrapper >> doesNotUnderstand: aMessage [
	^ aMessage sendTo: environment
]

{ #category : #'classes and traits' }
FLEnvironmentProtectingWrapper >> forgetClass: aBehavior logged: aBoolean [
	self
		removeKey: aBehavior name
		ifAbsent: []
]

{ #category : #testing }
FLEnvironmentProtectingWrapper >> includes: anObject [
	^ (overlay includes: anObject) or: [
		environment includes: anObject ]
]

{ #category : #testing }
FLEnvironmentProtectingWrapper >> includesKey: anObject [
	^ overlay
		at: anObject
		ifPresent: [ :value |
			value == sentinel
				ifTrue: [ false ]
				ifFalse: [ environment includesKey: anObject ] ]
		ifAbsent: [ environment includesKey: anObject ]
]

{ #category : #initialization }
FLEnvironmentProtectingWrapper >> initialize [
	overlay := SystemDictionary new.
	sentinel := Object new
]

{ #category : #initialization }
FLEnvironmentProtectingWrapper >> initializeWithEnvironment: anEnvironment [
	environment := anEnvironment
]

{ #category : #accessing }
FLEnvironmentProtectingWrapper >> keyAtIdentityValue: anObject ifAbsent: aBlock [
	^ overlay
		keyAtIdentityValue: anObject
		ifAbsent: [
			environment
				keyAtIdentityValue: anObject
				ifAbsent: aBlock ]
]

{ #category : #accessing }
FLEnvironmentProtectingWrapper >> keyAtIdentityValue: anObject ifPresent: presentBlock ifAbsent: absentBlock [
	^ overlay
		keyAtIdentityValue: anObject
		ifPresent: presentBlock
		ifAbsent: [
			environment
				keyAtIdentityValue: anObject
				ifPresent: presentBlock
				ifAbsent: absentBlock ]
]

{ #category : #accessing }
FLEnvironmentProtectingWrapper >> keyAtValue: anObject [
	^ overlay
		keyAtValue: anObject
		ifAbsent: [
			environment keyAtValue: anObject ]
]

{ #category : #accessing }
FLEnvironmentProtectingWrapper >> keyAtValue: anObject ifAbsent: aBlock [
	^ overlay
		keyAtValue: anObject
		ifAbsent: [
			environment
				keyAtValue: anObject
				ifAbsent: aBlock ]
]

{ #category : #binding }
FLEnvironmentProtectingWrapper >> lookupVar: aString [
	^ (overlay lookupVar: aString) ifNil: [
		environment lookupVar: aString ]
]

{ #category : #'classes and traits' }
FLEnvironmentProtectingWrapper >> removeKey: aKey [
	^ overlay
		at: aKey
		ifPresent: [
			overlay
				at: aKey
				put: sentinel ]
		ifAbsent: [
			environment
				at: aKey
				ifPresent: [
					overlay
						at: aKey
						put: sentinel ]
				ifAbsent: [
					KeyNotFound signalFor: aKey ] ]
]

{ #category : #'classes and traits' }
FLEnvironmentProtectingWrapper >> removeKey: aKey ifAbsent: aBlock [
	^ overlay
		at: aKey
		ifPresent: [ :global |
			overlay
				at: aKey
				put: sentinel ]
		ifAbsent: [
			environment
				at: aKey
				ifPresent: [ :global |
					overlay
						at: aKey
						put: sentinel ]
				ifAbsent: aBlock ]
]

{ #category : #renaming }
FLEnvironmentProtectingWrapper >> renameClass: aClass from: oldName [
	overlay
		at: aClass name put: aClass;
		at: oldName put: sentinel
]
