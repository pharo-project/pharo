Extension { #name : #MicEnvironmentBlock }

{ #category : #'*Microdown-Pillar' }
MicEnvironmentBlock >> asPillar [
	| classPillar |
	classPillar := PRAbstractAnnotation findClassAcceptingTag: name ifNone: [ PRUndefinedAnnotation ].
	^ classPillar new parameters: arguments
]
