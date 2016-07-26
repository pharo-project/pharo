serializing
storeSerializedAnnouncement
	"self storeSerializedAnnouncement"
	GTEventPacking all do: [ :eachEventPacking | | versionString |
		versionString := Symbol streamContents: [ :aStream |
			aStream 
				nextPutAll: #version;
				print: eachEventPacking version;
				nextPutAll: eachEventPacking technologyName ].
		self storeSerializedAnnouncementIntoMethod: versionString using: eachEventPacking  ]