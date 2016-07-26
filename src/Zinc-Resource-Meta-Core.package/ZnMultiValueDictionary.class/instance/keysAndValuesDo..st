enumerating
keysAndValuesDo: block
	"Execute a two argument (key, value) block for each header.
	Multi-valued headers are handled transparently."

	super keysAndValuesDo: [ :key :value |
		value isArray
			ifTrue: [ 
				value do: [ :each | 
					block value: key value: each ] ]
			ifFalse: [
				block value: key value: value ] ]