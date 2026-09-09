Class {
	#name : 'OCBehaviorWithExtensionInstaller',
	#superclass : 'Object',
	#instVars : [
		'behaviorInstaller'
	],
	#category : 'Kernel-CodeModel-Methods',
	#package : 'Kernel-CodeModel',
	#tag : 'Methods'
}

{ #category : 'accessing' }
OCBehaviorWithExtensionInstaller >> behaviorInstaller [

	^ behaviorInstaller
]

{ #category : 'accessing' }
OCBehaviorWithExtensionInstaller >> behaviorInstaller: anObject [

	behaviorInstaller := anObject
]

{ #category : 'accessing' }
OCBehaviorWithExtensionInstaller >> changeStamp: anObject [

	behaviorInstaller changeStamp: anObject
]

{ #category : 'installing' }
OCBehaviorWithExtensionInstaller >> install: method into: behavior compiler: compiler [

	| aliasingSelectors |
	behaviorInstaller install: method into: behavior compiler: compiler.
	
	"Wait! There was maybe an extension with the same name here!
	If that's the case, we need to mark the extension as a conflict."
	behavior selectors
		select: [ :e | e isScopedSelector and: [ e symbol = method selector ] ]
		thenDo: [ :selector | selector bindAliasIn: behavior ].
		
	"We are compiling a normal method.
	We need to alias it locally if superclasses are extended/aliased.
	If a subclass has a method with the same selector, this should have been aliased before"
	aliasingSelectors := (behavior allSuperclasses flatCollect: [ :aSuperclass |
		                      aSuperclass methodDict keys select: [ :selector |
			                      selector isScopedSelector and: [ selector asSymbol = method selector ] ] ]) asSet.
	aliasingSelectors do: [ :k | behavior methodDict at: k put: method ].
]

{ #category : 'accessing' }
OCBehaviorWithExtensionInstaller >> logged: anObject [

	behaviorInstaller logged: anObject
]

{ #category : 'accessing' }
OCBehaviorWithExtensionInstaller >> priorMethod: anObject [

	behaviorInstaller priorMethod: anObject
]

{ #category : 'accessing' }
OCBehaviorWithExtensionInstaller >> protocol: anObject [

	behaviorInstaller protocol: anObject
]
