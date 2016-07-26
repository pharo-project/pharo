accessing
unpackedData
	^ self data ifNotNil: [ :aData | 	
			self serializedBy asClass new
				migrations: self migrationRules;
				unpack: aData ]