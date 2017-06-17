initialization
fromPackage: package message: commitMessage
	"This creates a partial version info. Full version info can not be created until we commit into a repository, 
	but current fileout tools require it, so we first create an incomplete info and after committing it will be completed
	by sending #fromCommit:package:
	
	To improve this implementation we should review the fileout process."

	name := ('{1}-{2}.{3}' format: {package name. Author fullName. 'unknown'}).
	message := commitMessage.

