execute
execute
	"Remove from the configuration class what is selected in the versions panel list"

	| answer imported |
	imported := Set new.
	self versions do: [ :version | imported addAll: version importedVersions ].
	(imported includes: target versionString)
		ifTrue: [ 
			^ UIManager default
				inform:
					'The version ' , target printString
						, ' is imported by another version. You must remove the import reference before you can remove this version.' ].
	answer := UIManager default
		confirm: 'Are you sure you want to remove ' , target printString , ' from ' , self configurationClass name , '?'.
	answer
		ifTrue: [ 
			| cm |
			self configurationClass project symbolicVersionSymbols
				do: [ :symbolicVersion | 
						[ (MetacelloToolBox configurationNamed: self configurationClass name asString)
							modifySymbolicVersionMethodFor: symbolicVersion
								symbolicVersionSpecsDo: [ :symbolicVersionSpec | 
									symbolicVersionSpec versionString = target versionString
										ifTrue: [ symbolicVersionSpec versionString: #notDefined ].
									true ];
						commitMethod ]
					on: MetacelloValidationFailure 
					do: [ :error | "ignore" ] ].
			cm := MetacelloToolBox compiledMethodForVersion: target version.
			cm methodClass removeSelector: cm selector ]