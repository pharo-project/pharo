private
backpointersSetWith: anObject
	"Create a special set, because the host systems wrongly treats #abc and 'abc' as equal."
	
	^ PluggableSet new
		equalBlock: [ :a :b | a class == b class and: [ a = b ] ];
		hashBlock: [ :a | a class identityHash bitXor: a hash ];
		add: anObject;
		yourself