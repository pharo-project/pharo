loading
buildPatchFrom: oldDefinitions to: newDefinitions
	^ MCPatch
		fromBase: (MCSnapshot fromDefinitions: oldDefinitions)
		target: (MCSnapshot fromDefinitions: newDefinitions)