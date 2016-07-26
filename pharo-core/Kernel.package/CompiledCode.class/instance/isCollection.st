testing
isCollection
	"We should override it because most of collection methods are not working for CompiledMethod. It is special class which needs to be ByteArray by VM. But it is not behaves like ByteArray from user perspective.
	And some tools uses isCollection check for specific behaviour which will fail for CompiledMethod"
	^false