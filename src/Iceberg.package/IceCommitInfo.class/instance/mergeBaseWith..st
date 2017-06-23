querying
mergeBaseWith: anotherCommit
	^ self repository commitAt: 
		(self repository backend mergeBaseBetween: self id and: anotherCommit id)