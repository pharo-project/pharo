Extension { #name : #SystemDictionary }

{ #category : #'*Refactoring-Changes' }
SystemDictionary >> defineClass: aDefinition withController: aController [

	^ (self class compiler
		source: aDefinition;
		requestor: aController;
		logged: true;
		evaluate) fluidInstall
]
