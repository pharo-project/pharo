system startup
startUp  "DisplayScreen startUp"
	Display setExtent: self actualScreenSize depth: Display nativeDepth.
	Display beDisplay