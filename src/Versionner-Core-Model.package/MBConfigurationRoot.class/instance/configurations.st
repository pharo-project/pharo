accessing
configurations
	configurations
		ifNil: [ 
			configurations := (self configurationClasses
				collect: [ :cls | 
					(MBConfigurationInfo configurationClass: cls)
						configurationRoot: self;
						yourself ]) asSortedCollection: [ :a :b | a name <= b name ] ].
	^ configurations