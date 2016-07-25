*System-Sources
putSource: sourceStr class: class category: catName withStamp: changeStamp priorMethod: priorMethod
	^ self
		putSource: sourceStr
		withPreamble: [ :file | 
			class
				printCategoryChunk: catName
				on: file
				withStamp: changeStamp
				priorMethod: priorMethod.
			file cr ]