"
The rule detects if any Float system classes are refferenced by an AST
"
Class {
	#name : 'FloatReferencesRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Base',
	#package : 'General-Rules',
	#tag : 'Base'
}

{ #category : 'accessing' }
FloatReferencesRule class >> group [
	^ 'Bugs'
]

{ #category : 'accessing' }
FloatReferencesRule class >> ruleName [
	^ 'Access to a system class'
]

{ #category : 'running' }
FloatReferencesRule >> basicCheck: node [
	node isGlobalVariable ifFalse: [ ^ false ].
	^(self systemClassNames includes: node name)
]

{ #category : 'accessing' }
FloatReferencesRule >> rationale [

	^ 'You should not reference ',
		(self systemClassNames joinUsing: Character space),
		' as they are there for system purpose and should not be referenced directly.'
]

{ #category : 'running' }
FloatReferencesRule >> systemClassNames [

	^ #(BoxedFloat64 SmallFloat64)
]
