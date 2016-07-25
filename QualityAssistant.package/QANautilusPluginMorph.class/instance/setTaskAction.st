task handling
setTaskAction

	updateTask
		doInBackground: [
			critiques := entity
				ifNil: [ #() ]
				ifNotNil: [
					entity externalProperties asArray sort: [ :a :b | a title < b title ] ] ];
		onCompleted: [ self taskCompleted ]