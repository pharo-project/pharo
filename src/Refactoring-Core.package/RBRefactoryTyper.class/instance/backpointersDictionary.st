private
backpointersDictionary
	"Create a special dictionary, because the host systems wrongly treats #abc and 'abc' as equal."

	^ PluggableDictionary new
		equalBlock: [ :a :b | a class == b class and: [ a = b ] ];
		hashBlock: [ :a | a class identityHash bitXor: a hash ];
		yourself