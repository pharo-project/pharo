requesting
deleteBranch: owner project: projectName name: branchName
	^ self delete: 'repos/', owner, '/', projectName, '/git/refs/heads/', branchName