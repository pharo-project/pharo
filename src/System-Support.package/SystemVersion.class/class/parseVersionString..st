deprecated
parseVersionString: versionString 
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