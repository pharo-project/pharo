private
buildStreamFor: aCollection
	^ String
		streamContents:
			[ :stream | 
			stream
				nextPutAll: 'StartupPreferencesLoader default executeAtomicItems: {';
				crlf.
			aCollection
				do: [ :item | 
					stream
						tab;
						nextPutAll: item storeString;
						nextPut: $.;
						crlf ].
			stream
				nextPut: $};
				nextPut: $.;
				crlf;
				crlf ]