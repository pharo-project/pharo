ston persistence
fromSton: stonReader
	| array |
	array := Array new: 2 streamContents: [ :stream |
		stonReader parseListDo: [ :each :index | stream nextPut: each ] ].

	"Optimization: only write globalName if it belongs to a 'remote' store"
	^ array size = 1
		ifTrue: [ 
			self 
				globalName: OmCurrentGlobalName value
				localName: array first ]
		ifFalse: [
			self 
				globalName: array first
				localName: array second ]
