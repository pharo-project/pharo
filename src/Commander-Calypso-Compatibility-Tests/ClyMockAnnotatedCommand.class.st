"
I am a command placed with class annotations, which ClyCommandDeclarationGeneratorTest converts into a <calypsoCommands> declaration. My annotations only apply to ClyMockAnnotatedToolContext.
"
Class {
	#name : 'ClyMockAnnotatedCommand',
	#superclass : 'ClyCommand',
	#category : 'Commander-Calypso-Compatibility-Tests',
	#package : 'Commander-Calypso-Compatibility-Tests'
}

{ #category : 'activation' }
ClyMockAnnotatedCommand class >> mockContextMenuActivation [

	<classAnnotation>
	^ ClyContextMenuActivation
		  byItem: 'Mock'
		  of: ClyExtraMenuGroup
		  order: 42
		  for: ClyMockAnnotatedToolContext
]

{ #category : 'activation' }
ClyMockAnnotatedCommand class >> mockShortcutActivation [

	<classAnnotation>
	^ ClyShortcutActivation by: $e meta , $m meta for: ClyMockAnnotatedToolContext
]

{ #category : 'execution' }
ClyMockAnnotatedCommand >> execute [

	^ self
]
