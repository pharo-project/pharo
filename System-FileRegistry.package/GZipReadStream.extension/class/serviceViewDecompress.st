*System-FileRegistry
serviceViewDecompress

	^ SimpleServiceEntry 
				provider: self 
				label: 'View decompressed'
				selector: #viewContents:
				description: 'View decompressed' 
