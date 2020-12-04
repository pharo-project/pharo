Extension { #name : #MicAnnotationInlineBlock }

{ #category : #'*Microdown-Pillar' }
MicAnnotationInlineBlock >> asPillar [

	| argumentsSplitter |
	argumentsSplitter := MicArgumentsSplitter withString: substring.
	name := argumentsSplitter name.
	arguments := argumentsSplitter arguments.
	"Since an annotation is represented by specific classes depending on the tag, 
	we should dynamically find it."

	^ self associatedPillarClass new parameters: arguments
]

{ #category : #'*Microdown-Pillar' }
MicAnnotationInlineBlock >> associatedPillarClass [ 

	^  PRAbstractAnnotation findClassAcceptingTag: name ifNone: [ PRUndefinedAnnotation ]
	
]
