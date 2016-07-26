accessing
methodChanges

	| methodChangeDict |
	methodChangeDict := Dictionary new.
	changeRecords associationsDo:
		[:assn | | changeTypes |
		changeTypes := assn value methodChangeTypes.
		changeTypes isEmpty ifFalse: [methodChangeDict at: assn key put: changeTypes]].
	^ methodChangeDict