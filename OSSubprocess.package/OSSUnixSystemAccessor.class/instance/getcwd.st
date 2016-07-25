cwd
getcwd 
	"Returns the current working directory of the vmProcess"
	| buffer bytesRead |
	"Ideally, we should use a smaller buffer and then if the answer is NULL 
	and errno is ERANGE then try again with a larget buffer. But...since we cannot
	have access to errno.... we simply create a very large buffer that we trust that
	any path fill fit in it..."
	buffer := ExternalAddress allocate: 4096 + 1.
	[
	bytesRead := self primitiveGetcwd: buffer size: 4096 + 1.
	bytesRead ifNil: [ self perror: 'getcwd()'].
	] ensure: [ buffer free ].
	^ bytesRead
		