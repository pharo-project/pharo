accessing
entriesWithCodeChanges

	^ self entries select: [:each | each content isCodeChange ]