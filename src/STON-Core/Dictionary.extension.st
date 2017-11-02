Extension { #name : #Dictionary }

{ #category : #'*ston-core' }
Dictionary class >> fromSton: stonReader [
	"Instances of STON mapClass will be read directly and won't arrive here.
	Other (sub)classes will use this method."
	
	| dictionary |
	dictionary := self new.
	stonReader parseMapDo: [ :key :value |
		dictionary at: key put: value ].
	^ dictionary
]

{ #category : #'*ston-core' }
Dictionary >> stonOn: stonWriter [
	"Instances of STON mapClass will be encoded directly, without a class tag.
	Other (sub)classes will be encoded with a class tag and will use a map representation. "
	
	self class == STON mapClass
		ifTrue: [ 
			stonWriter writeMap: self ]
		ifFalse: [ 
			stonWriter 
				writeObject: self 
				do: [ stonWriter encodeMap: self ] ]
]

{ #category : #'*ston-core' }
Dictionary >> stonPostReferenceResolution [
	"When references were resolved in me, the hash of my keys might have changed.
	Check if I am still healthy and rehash me if not."
	
	self isHealthy ifFalse: [ self rehash ]
]
