as yet unclassified
map: aClassMirror with: aDefinition

	"We create a mapping between the real classes and their definitions for later"
	
	| classSideMirror |
	self assert: (aClassMirror target isKindOf: EPSimulatorHandle).
	(aClassMirror backend simulator objectMemory classTableRootObj) ifNotNil: [
		self assert: aClassMirror target asClassIndexHandle address notNil.
		aClassMirror target: (aClassMirror target asClassIndexHandle) ].

	mirrorDefinitionMap at: aClassMirror put: aDefinition.
	definitionMirrorMap at: aDefinition put: aClassMirror.

	classSideMirror := aClassMirror basicClass.
	"only valid for Spur simulator bootstrap by now"
	(aClassMirror backend simulator objectMemory classTableRootObj) ifNotNil: [
		self assert: classSideMirror target asClassIndexHandle address notNil.
		classSideMirror target: (classSideMirror target asClassIndexHandle) ].
	
	mirrorDefinitionMap at: classSideMirror put: aDefinition classSide.
	definitionMirrorMap at: aDefinition classSide put: classSideMirror.