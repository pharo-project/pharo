materializing
materializeFrom: aStream

	| aDecoder materialization header |
	aDecoder := 
		FLDecoder 
			on: aStream 
			migrations: migrations 
			globalEnvironment: globalEnvironment.
	self verifySignatureFrom: aDecoder.
	self verifyVersionFrom: aDecoder.
	header := self decodeHeaderWith: aDecoder.
	header executePreMaterializationActions.
	materialization := materializationFactory value: aDecoder.
	"This is useful because when the user materialize something, what it is answered is the materialization objet, and not the materializer. Hence, it is difficult to query the header (like asking the additionalObjects) because materializer is lost. Therefore, we also set the header to the materialization."
	materialization header: header. 
	"This is useful because the postMaterializationActions may need to have access to the state of the materialization, for example, to the root"
	header materialization: materialization.
	header executePostMaterializationActions.
	^ materialization 
	