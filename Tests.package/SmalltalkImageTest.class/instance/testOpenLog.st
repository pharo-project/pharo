testing
testOpenLog
	"It must be possible to write to the log even when the log file can''t be created."

	"Standard case where a file can be created with this name."
	self writeToLogWithFileNamed: 'file.log'.

	"Other cases where a file with the given name can not be created."
	self writeToLogWithFileNamed: ''.
	self writeToLogWithFileNamed: '.'.
