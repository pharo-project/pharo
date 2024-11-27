Extension { #name : 'ASTMethodNode' }

{ #category : '*Reflectivity' }
ASTMethodNode >> decreaseLinkCount [

	self propertyAt: #linkCount put: ((self linkCount - 1) max: 0)
]

{ #category : '*Reflectivity' }
ASTMethodNode >> hasOption: aSymbol for: aLink [
	"take the class and method overrides to options into account"
	^(self optionsFor: aLink) includes: aSymbol
]

{ #category : '*Reflectivity' }
ASTMethodNode >> increaseLinkCount [

	self propertyAt: #linkCount put: self linkCount + 1
]

{ #category : '*Reflectivity' }
ASTMethodNode >> linkCount [

	^self propertyAt: #linkCount ifAbsentPut: [ 0 ]
]

{ #category : '*Reflectivity' }
ASTMethodNode >> linkCount: aNumber [

	self propertyAt: #linkCount put: aNumber
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
