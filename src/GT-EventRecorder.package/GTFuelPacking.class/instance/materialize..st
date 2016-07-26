private
materialize: aByteArray
	| materializer stream gzstream materialization |
	stream := aByteArray asByteArray readStream. 
	gzstream := GZipReadStream on: stream.
	materializer := FLMaterializer newDefault.
	self initializeMaterializer: materializer.
	materialization := materializer materializeFrom: gzstream.
	gzstream close.
	stream close.
	^ materialization root.