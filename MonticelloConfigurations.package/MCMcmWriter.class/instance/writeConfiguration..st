writing
writeConfiguration: aConfiguration

	stream nextPut: $(.

	aConfiguration repositories do: [:ea | 
		stream cr.
		stream nextPutAll: 'repository '.
		(MCConfiguration repositoryToArray: ea) printElementsOn: stream].

	aConfiguration dependencies do: [:ea | 
		stream cr.
		stream nextPutAll: 'dependency '.
		(MCConfiguration dependencyToArray: ea) printElementsOn: stream].

	stream cr.
	stream nextPut: $).
	stream cr.