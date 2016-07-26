monticello menus behavior
viewChangesFor: mcpackage in: repo
	| workingCopy patch |

	workingCopy := mcpackage workingCopy.
	'Finding changes' displayProgressFrom: 0 to: 10 during:[:bar|
		bar current: 1.
		patch := workingCopy changesRelativeToRepository: repo.
		bar current: 3.
		patch isEmpty
			ifTrue: [
				workingCopy modified: false.
				bar current: 10.
				self inform: 'No changes' ]
			ifFalse: [
				workingCopy modified: true.
				bar current: 5.
				self
					viewChanges: patch
					from: workingCopy  packageName, ' (', workingCopy ancestry ancestorString, ')'
					to: ('Modified {1}' translated format: {workingCopy description})
					in: workingCopy ]]