attributes
defaultPlatformAttributes
	| attributes versionString |
	((Smalltalk respondsTo: #image) and: [ Smalltalk image respondsTo: #metacelloPlatformAttributes ])
		ifTrue: [ ^ Smalltalk image metacelloPlatformAttributes ].
	attributes := OrderedCollection with: #squeakCommon with: #pharo.
	Smalltalk
		at: #SystemVersion
		ifPresent: [ :cl | 
			versionString := cl current version.
			(((versionString beginsWith: 'Pharo-1') or: [ versionString beginsWith: 'PharoCore1' ])
				or: [ versionString beginsWith: 'Pharo1' ])
				ifTrue: [ 
					attributes add: #'pharo1.x'.
					((versionString beginsWith: 'Pharo-1.0') or: [ versionString beginsWith: 'PharoCore1.0' ])
						ifTrue: [ attributes add: #'pharo1.0.x' ]
						ifFalse: [ 
							((versionString beginsWith: 'Pharo-1.1') or: [ versionString beginsWith: 'Pharo1.1' ])
								ifTrue: [ attributes add: #'pharo1.1.x' ]
								ifFalse: [ 
									((versionString beginsWith: 'Pharo-1.2') or: [ versionString beginsWith: 'Pharo1.2' ])
										ifTrue: [ attributes add: #'pharo1.2.x' ]
										ifFalse: [ 
											(versionString beginsWith: 'Pharo1.3')
												ifTrue: [ attributes add: #'pharo1.3.x' ] ] ] ] ] ].
	^ attributes