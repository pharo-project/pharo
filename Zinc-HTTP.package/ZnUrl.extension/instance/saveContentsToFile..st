*Zinc-HTTP
saveContentsToFile: aFilename
	"Download and return a fileReference to the resource that I refer to.

	If something goes wrong, an exception will be signalled."
	
	" 'http://zn.stfx.eu/zn/numbers.txt' asZnUrl saveContentsToFile: Path * 'numbers.txt'. "
	
	^ self performOperation: #saveContentsToFile: with: aFilename