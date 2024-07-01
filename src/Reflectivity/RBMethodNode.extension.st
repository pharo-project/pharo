Extension { #name : 'RBMethodNode' }

{ #category : '*Reflectivity' }
RBMethodNode >> decreaseLinkCount [

	self propertyAt: #linkCount put: ((self linkCount - 1) max: 0)
]

{ #category : '*Reflectivity' }
RBMethodNode >> hasOption: aSymbol for: aLink [
	"take the class and method overrides to options into account"
	^(self optionsFor: aLink) includes: aSymbol
]

{ #category : '*Reflectivity' }
RBMethodNode >> increaseLinkCount [

	self propertyAt: #linkCount put: self linkCount + 1
]

{ #category : '*Reflectivity' }
RBMethodNode >> linkCount [

	^self propertyAt: #linkCount ifAbsentPut: [ 0 ]
]

{ #category : '*Reflectivity' }
RBMethodNode >> linkCount: aNumber [

	self propertyAt: #linkCount put: aNumber
]

{ #category : '*Reflectivity' }
RBMethodNode >> metaLinkOptions [
	^{
	#metaLinkOptionsFromClassAndMethod -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}
]

{ #category : '*Reflectivity' }
RBMethodNode >> metaLinkOptionsFromClassAndMethod [
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
RBMethodNode >> optionsFor: aLink [
	| options |
	options := aLink options copy.
	options addAll: self metaLinkOptionsFromClassAndMethod.
	^options
]
