private
runClasses: aCollectionOfClasses
	aCollectionOfClasses
		do:
			[ :class | 
			(self examplesForClass: class) do: [ :example | self runExample: example ] displayingProgress: class name.
			(FileSystem workingDirectory / 'gtExamplesAllTestedClasses.txt') writeStream
				setToEnd;
				cr;
				nextPutAll: class name;
				flush;
				close ]
		displayingProgress: 'Running examples of ' , aCollectionOfClasses size asString , ' classes ...'