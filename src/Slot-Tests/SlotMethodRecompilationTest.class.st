Class {
	#name : #SlotMethodRecompilationTest,
	#superclass : #SlotSilentTest,
	#instVars : [
		'class'
	],
	#category : #'Slot-Tests-ClassBuilder'
}

{ #category : #accessing }
SlotMethodRecompilationTest >> method [
	^ class >> #m1
]

{ #category : #accessing }
SlotMethodRecompilationTest >> methodBinding [
	^ self method classBinding
]

{ #category : #tests }
SlotMethodRecompilationTest >> reshapeClass [
	^ class := self
		make: [ :builder | 
			builder fillFor: class.
			builder slots: #(#iv) ]
]

{ #category : #running }
SlotMethodRecompilationTest >> setUp [
	super setUp.
	class := self make: [ :builder |
		builder traitComposition: TOne ].
	class compile: 'm1'
	
]

{ #category : #tests }
SlotMethodRecompilationTest >> testClassReshapeShouldKeepClassBinding [
	"Check that when a class is reshaped, the recompilation of the methods gives correct class bindings (in the literal array of the methods)"

	self reshapeClass.
	self assert: self methodBinding identicalTo: class binding
]

{ #category : #tests }
SlotMethodRecompilationTest >> testClassReshapeShouldKeepClassBindingForTraitMethods [
	"A method should have the same binding than its class even if it come from a trait"

	self reshapeClass.
	self assert: self traitMethodBinding identicalTo: class binding
]

{ #category : #tests }
SlotMethodRecompilationTest >> testMethodClassBindingShouldBeClassBinding [
	"A method should have the same binding than its class"

	self assert: self methodBinding identicalTo: class binding
]

{ #category : #tests }
SlotMethodRecompilationTest >> testTraitMethodClassBindingShouldBeClassBinding [
	"A method should have the same binding than its class even if it come from a trait"

	self assert: self traitMethodBinding identicalTo: class binding
]

{ #category : #accessing }
SlotMethodRecompilationTest >> traitMethod [
	^ class >> #one
]

{ #category : #accessing }
SlotMethodRecompilationTest >> traitMethodBinding [
	^ self traitMethod classBinding
]
