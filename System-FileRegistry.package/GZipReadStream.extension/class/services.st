*System-FileRegistry
services

	^ Array 
		with: self serviceViewDecompress
		with: self serviceDecompressToFile
	