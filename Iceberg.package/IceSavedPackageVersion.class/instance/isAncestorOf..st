comparing
isAncestorOf: commitId
	^ (self repository command: { 'merge-base'. commitId. self commitId }) trim = self commitId  
