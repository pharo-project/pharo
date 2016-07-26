adding
addAllMulti: keyedCollection
	"Note that we use #at:add:"

	keyedCollection keysAndValuesDo: [ :key :value | 
		self at: key add: value ]