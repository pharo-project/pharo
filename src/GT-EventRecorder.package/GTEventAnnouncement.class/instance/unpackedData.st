accessing
unpackedData
	^ self data ifNotNil: [ :aData | 
		(self class environment at: self serializedBy) new
			migrations: self migrationRules;
			unpack: aData ]