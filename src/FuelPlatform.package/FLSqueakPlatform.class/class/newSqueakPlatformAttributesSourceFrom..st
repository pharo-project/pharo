private-hacks-metacello
newSqueakPlatformAttributesSourceFrom: aCollection
	^ String streamContents: [ :stream | 
		stream nextPutAll: 'defaultPlatformAttributes
	| attributes versionString |
	attributes := OrderedCollection with: #squeakCommon with: #squeak.
	Smalltalk
		at: #SystemVersion
		ifPresent: [:cl | 
			versionString := cl current version.
			(versionString beginsWith: ''Squeak3.10'')
				ifTrue: [attributes add: #''squeak3.10.x''].
			(versionString beginsWith: ''Squeak4'')
				ifTrue: [attributes add: #''squeak4.x''.'.
		aCollection do: [ :version |
			stream
				nextPutAll: '(versionString beginsWith: ''';
				nextPutAll:  version;
				nextPutAll: ''') ifTrue: [ attributes add: #''';
				nextPutAll: version asLowercase;
				nextPutAll: '.x''] ifFalse: [ ' ].
		stream 
			nextPutAll: (String newFrom: ((1 to: aCollection size + 2) collect: [ :i | $] ]));
			nextPutAll: '. ^ attributes' ]