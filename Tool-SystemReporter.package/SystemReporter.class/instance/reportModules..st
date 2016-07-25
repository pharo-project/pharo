reporting
reportModules: aStream
	self header: 'Loaded VM Modules' on: aStream.
	Smalltalk vm listLoadedModules asSortedCollection do: [:each | aStream nextPutAll: each; cr].
	aStream cr.
	self header: 'VM Built-in Modules' on: aStream.
	Smalltalk vm listBuiltinModules asSortedCollection do: [:each | aStream nextPutAll: each; cr].

