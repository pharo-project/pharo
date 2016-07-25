interpreting-private
computeSpecFrom: aSymbol selector: aSelector
	| instance |

	instance := (aSymbol isSymbol and: [ aSymbol ~= #model ])
		ifTrue: [ | result |
			result := self convertSymbolOfClassToInstance: aSymbol.
			result isSpecContainer
				ifTrue: [ 
					result := result buildAdapterWithSpec.
					model addDependent: result ].
			result ]
		ifFalse: [ self class private_interpretASpec: aSymbol model: model selector: aSelector ].
		
	^ SpecWrapper
		instance: instance
		selector: aSelector