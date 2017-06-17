initialization
initializeFrom: aGitLogLine
	"Reads commit info out of a git commit log line, such as: '|f659e2851cb84eb6812b19e5a820c06fb509b0d5|Nicolás Passerini|2016-05-27 09:20:49 +0200|ea6c151|Updated test method with version 2'. 

	This lines are obtained with git log options: '--format=|%H|%an|%ai|%p|%s' as in IceRepository>>incomingCommits. "

	| elements |
	elements := $| split: aGitLogLine.
	self
		commitId: (elements at: 2);
		username: (elements at: 3);
		datetime: (DateAndTime readFrom: (elements at: 4) readStream );
		parentIds: ((Character space split: (elements at: 5) trim) select: #notEmpty);
		comment: (elements at: 6)
