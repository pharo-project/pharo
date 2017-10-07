attributes
defaultPlatformAttributes
	| attributes  |
	attributes := OrderedCollection with: #squeakCommon with: #squeak.
	Smalltalk
		at: #SystemVersion
		ifPresent: [:cl | |versionString|
			versionString := cl current version asString.
			" Major version "
			#(	'Squeak3.10' (#'squeak3.10.x' ())
				'Squeak4' (#'squeak4.x' (
					'Squeak4.1' #'squeak4.1.x'
					'Squeak4.2' #'squeak4.2.x'
					'Squeak4.3' #'squeak4.3.x'
					'Squeak4.4' #'squeak4.4.x'
					'Squeak4.5' #'squeak4.5.x'
					'Squeak4.6' #'squeak4.6.x'))
				'Squeak5' (#'squeak5.x' (
					'Squeak5.0' #'squeak5.0.x')))
			pairsDo: [:major :allAttributes |
				(versionString beginsWith: major) ifTrue: [
					attributes add: allAttributes first.
					allAttributes second ifNotEmpty: [:minorVersions |
						minorVersions pairsDo: [:minor :attribute |
							(versionString beginsWith: minor) ifTrue: [
								attributes add: attribute.
								^ attributes]]]]]]. 
	^ attributes