"
The rule detects if any Float system classes are refferenced by an AST
"
Class {
	#name : 'FloatReferencesRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Bugs',
	#package : 'General-Rules',
	#tag : 'Bugs'
}

{ #category : 'accessing' }
FloatReferencesRule class >> group [
	^ 'Bugs'
]

{ #category : 'accessing' }
FloatReferencesRule class >> rationale [

	^ 'You should not reference ' , (self systemClassNames joinUsing: Character space)
	  , ' as they are there for system purpose and should not be referenced directly.'
]

{ #category : 'accessing' }
FloatReferencesRule class >> ruleName [
	^ 'Access to a system class'
]

{ #category : 'running' }
FloatReferencesRule class >> systemClassNames [

	^ #(BoxedFloat64 SmallFloat64)
]

{ #category : 'running' }
FloatReferencesRule >> basicCheck: node [
	node isGlobalVariable ifFalse: [ ^ false ].
	^(self systemClassNames includes: node name)
]

{ #category : 'running' }
FloatReferencesRule >> systemClassNames [
	^ self class systemClassNames
]
