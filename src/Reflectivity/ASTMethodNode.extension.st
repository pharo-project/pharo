Extension { #name : 'ASTMethodNode' }

{ #category : '*Reflectivity' }
ASTMethodNode >> hasOption: aSymbol for: aLink [
	"take the class and method overrides to options into account"
	^(self optionsFor: aLink) includes: aSymbol
]

{ #category : '*Reflectivity' }
ASTMethodNode >> metaLinkOptions [
	^{
	#metaLinkOptionsFromClassAndMethod -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}
]

{ #category : '*Reflectivity' }
ASTMethodNode >> metaLinkOptionsFromClassAndMethod [
	| options |
	options := Set new.
	self methodClass
		compiledMethodAt: #metaLinkOptions
		ifPresent: [ :method |
			options
				parseOptions:
					((method valueWithReceiver: nil) asDictionary
						at: self selector
						ifAbsent: [ #() ]) ].
	"set meta link options defined per method"
	(self pragmas
		select: [ :pragmaNode | pragmaNode selector == #metaLinkOptions: ])
		do:
			[ :pragmaNode | (pragmaNode pragma copy selector: #parseOptions:) sendTo: options ].
	^ options
]

{ #category : '*Reflectivity' }
ASTMethodNode >> optionsFor: aLink [
	| options |
	options := aLink options copy.
	options addAll: self metaLinkOptionsFromClassAndMethod.
	^options
]
