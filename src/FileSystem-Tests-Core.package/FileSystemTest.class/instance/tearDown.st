running
tearDown
	toDelete
		select: [ :path | filesystem exists: path ]
		thenDo: [ :path | filesystem delete: path ]