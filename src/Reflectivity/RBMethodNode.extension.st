Extension { #name : #RBMethodNode }

{ #category : #'*Reflectivity' }
RBMethodNode >> hasOption: aSymbol for: aLink [
	"take the class and method overrides to options into account"
	^(self optionsFor: aLink) includes: aSymbol.
]

{ #category : #'*Reflectivity' }
RBMethodNode >> metaLinkOptions [
	^{
	#metaLinkOptionsFromClassAndMethod -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}
]

{ #category : #'*Reflectivity' }
RBMethodNode >> metaLinkOptionsFromClassAndMethod [
	| options |
	options := Set new.
	self methodClass
		compiledMethodAt: #metaLinkOptions
		ifPresent: [ :method | 
			options
				parseOptions:
					((method valueWithReceiver: nil arguments: #()) asDictionary
						at: self selector
						ifAbsent: [ #() ]) ].
	"set meta link options defined per method"
	(self pragmas
		select: [ :pragma | pragma selector == #metaLinkOptions: ])
		do:
			[ :pragma | (pragma asPragma selector: #parseOptions:) sendTo: options ].
	^ options
]

{ #category : #'*Reflectivity' }
RBMethodNode >> optionsFor: aLink [
	| options |
	options := aLink options copy.
	options addAll: self metaLinkOptionsFromClassAndMethod.
	^options
	
]
