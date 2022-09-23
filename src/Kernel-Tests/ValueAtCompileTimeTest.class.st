Class {
	#name : #ValueAtCompileTimeTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Methods'
}

{ #category : #tests }
ValueAtCompileTimeTest >> testValueAtCompileTime [

	|v|
	
	v := [ 1+2 ] valueAtCompileTime.
	
	self assert: v equals: 3
]

{ #category : #tests }
ValueAtCompileTimeTest >> testValueAtCompileTimeLocalTempsAreOK [

	|v|
	
	v := [ |x| x:= 1. { x. x+1 } ] valueAtCompileTime.
	
	self assert: v equals: { 1. 2 }
]

{ #category : #tests }
ValueAtCompileTimeTest >> testValueAtCompileTimeNoLiteralBlock [

	|b|
	
	"Only a literal block is accepted"
	
	self flag: 'FIXME: add compiler errors or warnings'.

	b := [ {1+2} ].
	
	self should: [ b valueAtCompileTime ] raise: Error
]

{ #category : #tests }
ValueAtCompileTimeTest >> testValueAtCompileTimeNoParameters [

	|v|
	
	"Parameters are not accepted"
	
	self flag: 'FIXME: add compiler errors or warnings'.
	
	v := [ :x | {x} ] valueAtCompileTime.
	
	self assert: v equals: { nil }
]

{ #category : #tests }
ValueAtCompileTimeTest >> testValueAtCompileTimeReuse [

	"The object evaluated at compile time is reused and mutable.
	User discretion is advised."

	| res |

	1 to: 5 do: [ :i | "The same ordered collection will be reused."
		res := [ { 0 } asOrderedCollection ] valueAtCompileTime.

		"Force a reset at the first loop to avoid polution from unrelated executions."
		i = 1 ifTrue: [ res removeAll ].

		res add: i ].

	"For information, if `value` was user instead of `valueAtCompileTime`, res will be {0.5}"
	self assert: res asArray equals: { 1. 2. 3. 4. 5 }
]

{ #category : #tests }
ValueAtCompileTimeTest >> testValueAtCompileTimeSpeedup [

	|b1 b2|

	b1 := [ [ 100 factorial ] value ] benchFor: 0.01second.
	b2 := [ [ 100 factorial ] valueAtCompileTime ] benchFor: 0.01second.
	
	"x100 is very conservative. But if for some reason the evaluation is not done a compile time, this should detect it."
	self assert: (100 * b1 executionsPerSecond) < (b2 executionsPerSecond)
]

{ #category : #tests }
ValueAtCompileTimeTest >> testValueAtCompileTimeUnbinded [

	|v x|
	
	x := 4.
	
	"Values are not bound inside the block."
	
	self flag: 'FIXME: add compiler errors or warnings'.
	
	v := [ {x. self} ] valueAtCompileTime.
	
	self assert: v equals: { nil. nil }
]
