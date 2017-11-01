Class {
	#name : #EyeInspectorTest,
	#superclass : #TestCase,
	#category : 'Spec-Inspector-Test'
}

{ #category : #accessing }
EyeInspectorTest >> instancesToTry [
	^ { 
	Object new . 
	Class . 
	Metaclass . 
	Array new .
	Dictionary new . 
	WordArray new. 
	ByteArray new. 
	Color red .
	thisContext . 
	12 . 
	#symbol . 
	(12.32) . 
	TBehavior . 
	(SmallInteger>>#+) . 
	#(1 2 3) . 
	#[1 2 3] .
	[  1 + 2 ] .
	Error new .
	(1 << 40)
	}
]

{ #category : #tests }
EyeInspectorTest >> testErrorRaised [
	| inspector morphsBefore morphsAfter |
	morphsBefore := World submorphs. 
	self instancesToTry do: [ :each | inspector := EyeInspector inspector: each].
	morphsAfter := World submorphs. 
	(morphsAfter copyWithoutAll: morphsBefore) do: [ :morph | morph delete ].
]
