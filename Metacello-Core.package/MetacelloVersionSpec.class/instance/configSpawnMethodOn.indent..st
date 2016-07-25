printing
configSpawnMethodOn: aStream indent: indent

	self configMethodValueOn: aStream for: self getBlessing selector: 'blessing:' last: false indent: indent.
	self configMethodValueOn: aStream for: self getAuthor selector: 'author:' last: false indent: indent.
	self configMethodValueOn: aStream for: self getTimestamp selector: 'timestamp:' last: false indent: indent.