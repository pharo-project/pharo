serializing
storeSerializedAnnouncementIntoMethod: aSymbol using: aGTEventRecorder
	"self storeSerializedAnnouncementIntoMethod: #version3 using: GTEventRecorder default."
	| methodSource |
	methodSource := (String streamContents: [ :aStream |
		aStream 
			nextPutAll: aSymbol asString; cr; 
			tab; 
			nextPutAll: '"Automatically created by class method #storeSerializedAnnouncementIntoMethod: ';
			print: aSymbol;
			nextPutAll: ' using: aGTEventRecorder ';
			nextPutAll: aGTEventRecorder technologyName;
			nextPut: $.;
			cr; tab;
			nextPutAll: 'It is a serialized instance of GTEventAnnouncement.';
			cr; tab;
			nextPutAll: 'It contains all instances of the Spotter recorder events."';
			cr; tab;
			nextPutAll: '<spotterEventRecorderSerializedData>';
			cr; tab;
			nextPutAll: '^ '.
		(self serializeAnnouncementWithAllEventsUsing: aGTEventRecorder) storeOn: aStream. ]).
	self compile: methodSource classified: 'serialized data'.