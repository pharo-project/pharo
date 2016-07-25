enumerating
keysAndValuesDo: block
	"Execute block with each key and value present in me.
	This will be from least to most recently used."
	
	lruList do: [ :link |
		block 
			value: link key 
			value: link value ]