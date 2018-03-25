Extension { #name : #SystemVersion }

{ #category : #'*Deprecated70' }
SystemVersion class >> fromShortVersionString: aString [
	"Create a new SystemVersion from the given short version string. Useful for testing."

	| parts |	
	self 
		deprecated: 'Use #fromVersionString: instead'
		on: 	'22 September 2017' 
		in: 'Pharo-7.0+alpha.build.148.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8'.	
		
	parts := self parseShortVersionString: aString.
	^self new
		type: parts first;
		major: parts second;
		minor: parts third;
		suffix: parts fourth;
		yourself
]

{ #category : #'*Deprecated70' }
SystemVersion class >> newVersion: versionName [
	"Install a new SystemVersion as the current version. Only preserve the higest update recorded.
	This method is a candidate for being removed, I am not sure it ever makes sense to do this."
	
	| newVersion |
	
	self 
		deprecated: 'Method will be removed'
		on: 	'22 September 2017' 
		in: 'Pharo-7.0+alpha.build.148.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8'.
		
	newVersion := self fromShortVersionString: versionName.
	newVersion
		highestUpdate: self current highestUpdate.
	Current := newVersion

]

{ #category : #'*Deprecated70' }
SystemVersion class >> parseShortVersionString: aString [ 
	"Answer the parts of aString, expected in short format <type><major>.<minor><suffix>."

	| stream length type major minor suffix |

	self 
		deprecated: 'Use #fromVersionString: instead'
		on: 	'22 September 2017' 
		in: 'Pharo-7.0+alpha.build.148.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8'.
	
	stream := aString readStream.
	[stream peek isDigit] whileFalse: [stream next].
	length := stream position.
	stream reset.
	type := (stream next: length).
	major := (stream upTo: $.) asInteger.
	stream := stream upToEnd readStream.
	[stream peek notNil and: [stream peek isDigit]] whileTrue: [stream next].
	length := stream position.
	stream reset.
	minor := (stream next: length) asInteger.
	suffix := stream upToEnd.
	^{type. major. minor. suffix}
]

{ #category : #'*Deprecated70' }
SystemVersion class >> parseVersionString: versionString [ 
	"Answer the version of this release as version, date, update."
	"SystemVersion parseVersionString: 'Squeak3.1alpha of 28 February 2001 [latest update: #3966]' "
	| version date update |
	
	self 
		deprecated: 'Use #fromVersionString: instead'
		on: 	'22 September 2017' 
		in: 'Pharo-7.0+alpha.build.148.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8'.
	
	[ | stream | stream := versionString readStream.
	version := stream upToAll: ' of '.
	date := Date readFrom: stream.
	stream upToAll: ' #'.
	update := Number readFrom: stream ] 
		on: Error
		do: [ ^ nil ].
	^ {  version. date. update  }
]
