private-hacks-metacello
fixPlatformAttributes
	| versions |
	versions := { 'Squeak4.1'. 'Squeak4.2'. 'Squeak4.3'. 'Squeak4.4'. 'Squeak4.5'. 'Squeak4.6' }.
		
	Smalltalk 
		at: #MetacelloSqueakPlatform
		ifPresent: [ :platform |
			(Smalltalk at: #MetacelloSqueakPlatform) compile: (self newSqueakPlatformAttributesSourceFrom: versions) ]