accessing-computed
versions
	"Return the versions of the configuration"
	| versionList devVersionString |
	
	versions ifNotNil: [ versions ifNotEmpty: [ ^ versions ] ].
	versionList := self project map values.
	devVersionString := self project symbolicVersionMap at: #development ifAbsent: [ '' ].
	versionList reject: [ :vrsn | (vrsn blessing == #baseline and: [(vrsn name == devVersionString) not]) or: [ vrsn blessing == #broken ] ].
	
	versions := (versionList asArray sort: [:a :b | a > b ])
		collect: [ :vrsn | 
			(MBVersionInfo version: vrsn)
				configurationInfo: self;
				yourself ].
	^ versions