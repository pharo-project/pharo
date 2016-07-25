help-text
helpTextFor: aRPackage
	"We try to get a configuration class with the same name of the package"

	| possibleConfigurationNames |
	aRPackage name ifEmpty: [ ^ nil ].
	possibleConfigurationNames := OrderedCollection new.
	(aRPackage name beginsWith: 'ConfigurationOf')
		ifTrue: [ possibleConfigurationNames addLast: aRPackage name ]
		ifFalse: [ 
			| stream |
			"'ConfigurationOf' size"
			stream := (String new: aRPackage name size + 15) writeStream.
			stream nextPutAll: 'ConfigurationOf'.
			$-
				split: aRPackage name
				do: [ :each | 
					(each = 'Core' or: [ each = 'Tests' ])
						ifFalse: [ 
							stream nextPutAll: each.
							possibleConfigurationNames addLast: stream contents ] ] ].
	"try longeset match first"
	possibleConfigurationNames
		reverseDo: [ :each | 
			each asSymbol
				asClassIfPresent: [ :configuration | 
					"If the configuration has a catalog description we show it, if not we don't"
					(configuration respondsTo: #catalogDescription)
						ifTrue: [ ^ configuration catalogDescription ]
						ifFalse: [ ^ nil ] ] ].
	^ nil.