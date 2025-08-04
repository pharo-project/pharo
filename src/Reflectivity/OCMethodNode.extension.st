Extension { #name : 'OCMethodNode' }

{ #category : '*Reflectivity' }
OCMethodNode >> decreaseLinkCount [

	self propertyAt: #linkCount put: ((self linkCount - 1) max: 0)
]

{ #category : '*Reflectivity' }
OCMethodNode >> hasOption: aSymbol for: aLink [
	"take the class and method overrides to options into account"

	^ (self optionsFor: aLink) includesOption: aSymbol
]

{ #category : '*Reflectivity' }
OCMethodNode >> increaseLinkCount [

	self propertyAt: #linkCount put: self linkCount + 1
]

{ #category : '*Reflectivity' }
OCMethodNode >> linkCount [

	^self propertyAt: #linkCount ifAbsentPut: [ 0 ]
]

{ #category : '*Reflectivity' }
OCMethodNode >> linkCount: aNumber [

	self propertyAt: #linkCount put: aNumber
]

{ #category : '*Reflectivity' }
OCMethodNode >> metaLinkOptions [
	^{
	#metaLinkOptionsFromClassAndMethod -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}
]

{ #category : '*Reflectivity' }
OCMethodNode >> metaLinkOptionsFromClassAndMethod [
	| options |
	options := OCCompilationContext new.
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
OCMethodNode >> optionsFor: aLink [

	| options |
	options := aLink options copy.
	options addAllFrom: self metaLinkOptionsFromClassAndMethod.
	^ options
]
