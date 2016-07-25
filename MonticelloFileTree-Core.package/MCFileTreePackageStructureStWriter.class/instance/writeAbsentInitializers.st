writing
writeAbsentInitializers
	(self absentInitializers asSortedCollection: [ :a :b | a key <= b key ])
		do: [ :association | stream nextPutAll: association value contents ]