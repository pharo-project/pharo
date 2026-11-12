"
I stand for a context class annotated with the deprecated ClyContextSelectionStrategyAnnotation, to test that the annotation is still applied. My browser is ClyMockAnnotatedToolContext, which no browser is.
"
Class {
	#name : 'ClyMockAnnotatedSelectionContext',
	#superclass : 'Object',
	#category : 'Commander-Calypso-Compatibility-Tests',
	#package : 'Commander-Calypso-Compatibility-Tests'
}

{ #category : 'annotations' }
ClyMockAnnotatedSelectionContext class >> mockSelectionStrategy [

	<classAnnotation>
	^ (ClyContextSelectionStrategyAnnotation for: ClyMockAnnotatedToolContext selectionType: ClyClass) priority: 7
]
