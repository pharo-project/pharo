Extension { #name : 'SystemDictionary' }

{ #category : '*Refactoring-Changes' }
SystemDictionary >> defineClass: aDefinition [

	^ (self class compiler
			source: aDefinition;
			logged: true;
			evaluate) fluidInstall
]
