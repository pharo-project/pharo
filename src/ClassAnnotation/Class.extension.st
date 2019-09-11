Extension { #name : #Class }

{ #category : #'*ClassAnnotation' }
Class >> asAnnotationContext [
	^SimpleAnnotationContext of: self
]

{ #category : #'*ClassAnnotation' }
Class >> classAnnotationAt: selector [
	^self classAnnotations 
		detect: [ :each | each declarationSelector = selector ]
]

{ #category : #'*ClassAnnotation' }
Class >> classAnnotations [
	^ClassAnnotation findAllAnnotationsFor: self
]

{ #category : #'*ClassAnnotation' }
Class >> classAnnotationsDo: aBlock [
	^(ClassAnnotation findAllAnnotationsFor: self) do: aBlock
]
