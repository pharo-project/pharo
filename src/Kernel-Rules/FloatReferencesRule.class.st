"
The rule detects if any Float system classes are refferenced by an AST
"
Class {
	#name : #FloatReferencesRule,
	#superclass : #ReBasicTreeRule,
	#category : #'Kernel-Rules'
}

{ #category : #running }
FloatReferencesRule >> basicCheck: anEntity [
	
	 self
		check: anEntity
		forCritiquesDo: [ ^ true ]
		ifNone: [ ^ false ]
]

{ #category : #running }
FloatReferencesRule >> check: anAST forCritiquesDo: aCriticBlock ifNone: alternativeBlock [

	| encounteredCritique |
	encounteredCritique := false.

	anAST nodesDo: [ :node |
		(node isVariable and: [
		node binding ifNil: [ node doSemanticAnalysis ].
		node isGlobal and: [ 
		self systemClassNames includes: node name ] ])
			ifTrue: [
				encounteredCritique := true.
				aCriticBlock cull: (ReTrivialCritique
					withAnchor: (ReIntervalSourceAnchor
						entity: anAST
						interval: node sourceInterval)
					by: self) ] ].

	encounteredCritique ifFalse: alternativeBlock	
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
