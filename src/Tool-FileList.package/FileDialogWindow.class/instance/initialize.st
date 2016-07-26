initialization
initialize
	"Initialize the receiver."

	selectedFileIndex := 0.
	fileNameText := ''.
	self
		answerPathName;
		directories: self initialDirectories;
		showDirectoriesInFileList: true;
		fileSelectionBlock: self defaultFileSelectionBlock;
		fileSortBlock: self defaultFileSortBlock.
	super initialize. "Must be at end or signals error. No idea why - SeanDeNigris"