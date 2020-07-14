Extension { #name : #MicAnnotationInlineBlock }

{ #category : #'*Microdown-Pillar' }
MicAnnotationInlineBlock >> asPillar [
	| argumentsSplitter classPillar |
	
	argumentsSplitter := MicArgumentsSplitter withString: substring.
	name := argumentsSplitter name.
	arguments := argumentsSplitter arguments.
	
	classPillar := PRAbstractAnnotation findClassAcceptingTag: name ifNone: [ PRUndefinedAnnotation ].
	^ classPillar new parameters: arguments
]
