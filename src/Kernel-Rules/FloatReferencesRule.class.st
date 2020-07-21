"
The rule detects if any Float system classes are refferenced by an AST
"
Class {
	#name : #FloatReferencesRule,
	#superclass : #ReNodeBasedRule,
	#category : #'Kernel-Rules'
}

{ #category : #running }
FloatReferencesRule >> basicCheck: node [
	node isVariable ifFalse: [ ^ false ].
	node isGlobalVariable ifFalse: [ ^ false ].
	^(self systemClassNames includes: node name)
	
]

{ #category : #accessing }
FloatReferencesRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
FloatReferencesRule >> name [

	^ 'Access to a system class'
]

{ #category : #accessing }
FloatReferencesRule >> rationale [

	^ 'You should not reference ',
		(self systemClassNames joinUsing: Character space),
		' as they are there for system purpose and should not be referenced directly.'
]

{ #category : #running }
FloatReferencesRule >> systemClassNames [

	^ #(BoxedFloat64 SmallFloat64)
]
