git querying
gitVersionsForPackage: aDirectoryEntry
	"Query git for all the commits related to that directory entry. Typical use: the package top-level directory entry, i.e. <pathOfRepository>/<packageName>.package. Return a stream of git sha1, the commit author, the date and time of commit, and the message for each commit changing something in this directory entry."

	| packageDirectory |
	packageDirectory := self fileUtils directoryFromPath: aDirectoryEntry relativeTo: directory.
	^ ReadStream on: (self gitCommand: #('log' '--log-size' '--parents' '--reverse' '--format=|%H|%an|%ai|%p|%s' '--' '.') in: packageDirectory)