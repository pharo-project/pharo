registration
recentToolsFor: aName
	^ (recentTools at: aName ifAbsent: [ {} ]) reject: [ :tool | tool isObsolete ]