initialize-release
streamToMethod: aCompiledMethod
	stream := WriteStream with: aCompiledMethod.
	stream position: aCompiledMethod initialPC - 1