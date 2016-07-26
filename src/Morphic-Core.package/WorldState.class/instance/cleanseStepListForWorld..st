stepping
cleanseStepListForWorld: aWorld
	"Remove morphs from the step list that are not in this World"

	| deletions morphToStep |
	deletions := nil.
	stepList do: [:entry |
		morphToStep := entry receiver.
		morphToStep world == aWorld ifFalse:[
			deletions ifNil: [deletions := OrderedCollection new].
			deletions addLast: entry]].

	deletions ifNotNil:[
		deletions do: [:entry|
			self stopStepping: entry receiver]].

	self alarms copy do:[:entry|
		morphToStep := entry receiver.
		(morphToStep isMorph and:[morphToStep world == aWorld]) 
			ifFalse:[self removeAlarm: entry selector for: entry receiver]].