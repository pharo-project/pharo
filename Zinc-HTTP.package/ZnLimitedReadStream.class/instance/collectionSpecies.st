accessing
collectionSpecies
	"I can be set to binary or ascii explicitely.
	If not set, delegate to our wrapped stream."
	
	^ binary notNil
		ifTrue: [ 
			binary ifTrue: [ ByteArray ] ifFalse: [ ByteString ] ]
		ifFalse: [
			(stream respondsTo: #collectionSpecies)
				ifTrue: [ stream collectionSpecies ]  
				ifFalse: [ stream isBinary ifTrue: [ ByteArray ] ifFalse: [ ByteString ] ] ]