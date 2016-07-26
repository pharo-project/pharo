initialization
buttonSelectorsToSuppress
	"Answer a list of action selectors whose corresponding services we would prefer *not* to have appear in the filelist's button pane; this can be hand-jimmied to suit personal taste."

	^ #(removeLineFeeds: addFileToNewZip: compressFile:)