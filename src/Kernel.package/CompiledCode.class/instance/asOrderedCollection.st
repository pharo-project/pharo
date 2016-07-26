converting
asOrderedCollection
	"We should override it because most of collection methods are not working for CompiledMethod. And it can't be normally converted into OrderedCollection.	
	It is special class which needs to be ByteArray by VM. But it is not behaves like ByteArray from user perspective"
	
	^OrderedCollection with: self