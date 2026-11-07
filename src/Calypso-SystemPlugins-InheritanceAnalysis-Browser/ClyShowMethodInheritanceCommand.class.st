"
I am a command to show all method implementors in full hierarchy of selected method classes.
I select current method in spawned browser by default.
"
Class {
	#name : 'ClyShowMethodInheritanceCommand',
	#superclass : 'ClyShowLocalImplementorsCommand',
	#category : 'Calypso-SystemPlugins-InheritanceAnalysis-Browser',
	#package : 'Calypso-SystemPlugins-InheritanceAnalysis-Browser'
}

{ #category : 'activation' }
ClyShowMethodInheritanceCommand class >> browserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: $h meta for: ClyMethod asCalypsoItemContext
]

{ #category : 'execution' }
ClyShowMethodInheritanceCommand >> createInheritanceScopeFrom: classes [

	| classScope |
	classScope := ClyClassScope ofAll: classes in: browser navigationEnvironment.

	^classScope asFullHierarchyScope
]

{ #category : 'accessing' }
ClyShowMethodInheritanceCommand >> description [

	^ 'Show all method implementors in full hierarchy of selected method classes.'
]

{ #category : 'accessing' }
ClyShowMethodInheritanceCommand >> iconName [

	^ #browse
]

{ #category : 'accessing' }
ClyShowMethodInheritanceCommand >> name [
	^'Hierarchical implementors'
]

{ #category : 'accessing' }
ClyShowMethodInheritanceCommand >> order [

	^ 2
]
