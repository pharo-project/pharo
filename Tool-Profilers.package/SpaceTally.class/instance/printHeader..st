fileout
printHeader: aStream
	aStream
		nextPutAll: ('Class' padRightTo: 45);
		nextPutAll: ('code space' padLeftTo: 12);
		nextPutAll: ('# instances' padLeftTo: 12);
		nextPutAll: ('inst space' padLeftTo: 12);
		nextPutAll: ('percent' padLeftTo: 12);
		nextPutAll: ('inst average size' padLeftTo: 20);
		cr