taskbar
updateTasks
	"Check for changes in the world's submorphs.
	Note that if the task attributes change then a
	task will be considered dead along with a new replacement."
	
	|wm deadTasks newTasks|
	wm := self worldMorphs.
	self tasks: ((wm collect: [:m | m taskbarTask]) select: [:m | m notNil]) asOrderedCollection.
	deadTasks := self orderedTasks difference: self tasks.
	newTasks := self tasks difference: self orderedTasks.
	(newTasks isEmpty and: [deadTasks isEmpty])
		ifTrue: [^self]. "no changes"
	newTasks copy do: [:t |
		self orderedTasks detect: [:ot | ot morph = t morph] ifFound: [:ot |
			self orderedTasks replaceAll: ot with: t.
			deadTasks remove: ot.
			newTasks remove: t]]. "replace in order any changed tasks."
	self orderedTasks
		removeAll: deadTasks;
		addAll: newTasks reversed.
	self updateTaskButtons.
	self defer: [self layoutChanged] "may have a different number of rows"