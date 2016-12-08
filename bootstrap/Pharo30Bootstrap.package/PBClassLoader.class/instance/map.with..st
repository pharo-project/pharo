registration
map: aClassMirror with: aDefinition

	"We create a mapping between the real classes and their definitions for later"
	
	mirrorDefinitionMap at: aClassMirror put: aDefinition.
	definitionMirrorMap at: aDefinition put: aClassMirror.
	
	mirrorDefinitionMap at: aClassMirror basicClass put: aDefinition classSide.
	definitionMirrorMap at: aDefinition classSide put: aClassMirror basicClass