private
parseCommitInfo: aLGitCommit
	^ IceCommitInfo new
		commitId: aLGitCommit id hexString;
		username: aLGitCommit committer name;
		datetime: aLGitCommit time asDateAndTime;
		parentIds: (aLGitCommit parents collect: [ :parent | parent id hexString ]);
		comment: aLGitCommit message