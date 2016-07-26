private
validateVersionSpecForSymbolicVersion: versionSpec symbolicVersion: symbolicVersionString
	| blessing |
	versionSpec blessing value == #broken
		ifTrue: [ ^ self ].
	blessing := versionSpec blessing value.
	(symbolicVersionString == #development and: [ blessing ~~ #development ])
		ifTrue: [ 
			self
				recordValidationWarning:
					'Symbolic version ' , symbolicVersionString printString , ' refers to a version' , versionSpec versionString printString
						, ' whose blessing ' , blessing printString , ' is not #development'
				versionString: versionSpec versionString
				callSite: #validateVersionSpecForSymbolicVersion:symbolicVersion:
				reasonCode: #notDevelopmentVersion ]