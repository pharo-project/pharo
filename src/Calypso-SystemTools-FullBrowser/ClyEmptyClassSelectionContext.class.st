Class {
	#name : #ClyEmptyClassSelectionContext,
	#superclass : #AnnotationContext,
	#category : #'Calypso-SystemTools-FullBrowser-Contexts'
}

{ #category : #testing }
ClyEmptyClassSelectionContext >> describes: aBrowserContext [
	^(aBrowserContext isKindOf: ClyFullBrowserContext) 
		and: [aBrowserContext isClassSelected not]
]
