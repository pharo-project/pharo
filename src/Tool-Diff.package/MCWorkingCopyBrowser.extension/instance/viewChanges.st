*Tool-Diff
viewChanges
	"View the changes made in the working copy."
	
	| patch |
	'Finding changes' displayProgressFrom: 0 to: 10 during:[:bar|
		self canSave ifTrue:[
		bar current: 1.
		patch := workingCopy changesRelativeToRepository: self repository].
		patch ifNil: [^ self].
		bar current: 3.
		patch isEmpty
			ifTrue: [
				workingCopy modified: false.
				bar current: 10.
				MCNoChangesException signal ]
			ifFalse: [
				workingCopy modified: true.
				bar current: 5.
				self
					viewChanges: patch
					from: workingCopy  packageName, ' (', workingCopy ancestry ancestorString, ')'
					to: ('Modified {1}' translated format: {workingCopy description})]]