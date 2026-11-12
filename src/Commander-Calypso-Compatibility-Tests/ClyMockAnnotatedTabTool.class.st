"
I am a tool annotated with the deprecated ClyTabActivationStrategyAnnotation, to test that the annotation is still applied.
"
Class {
	#name : 'ClyMockAnnotatedTabTool',
	#superclass : 'Object',
	#category : 'Commander-Calypso-Compatibility-Tests',
	#package : 'Commander-Calypso-Compatibility-Tests'
}

{ #category : 'testing' }
ClyMockAnnotatedTabTool class >> isAbstract [

	^ false
]

{ #category : 'annotations' }
ClyMockAnnotatedTabTool class >> mockTabActivation [

	<classAnnotation>
	^ ClyTabActivationStrategyAnnotation for: ClyMockAnnotatedToolContext
]

{ #category : 'testing' }
ClyMockAnnotatedTabTool class >> shouldBeActivatedInContext: aToolContext [

	^ true
]
