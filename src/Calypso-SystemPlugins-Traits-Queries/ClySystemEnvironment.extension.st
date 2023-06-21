Extension { #name : #ClySystemEnvironment }

{ #category : #'*Calypso-SystemPlugins-Traits-Queries' }
ClySystemEnvironment >> defineTrait: defString notifying: aController startingFrom: aClass [

	| trait |
	trait := self defaultClassCompiler
		source: defString; requestor: aController;
		logged: true;
		evaluate.
		
	"mild ugly hack for fluid traits"
	(trait isKindOf: FluidTraitBuilder) ifTrue: [ trait := trait fluidInstall ].

	^trait isTrait ifTrue: [ trait ] ifFalse: [ nil ]
]
