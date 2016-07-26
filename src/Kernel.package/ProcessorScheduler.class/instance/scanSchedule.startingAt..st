accessing
scanSchedule: aBlock startingAt: topPriority
	"Iterate over scheduled processes  list, starting from topPriority down to lowest one. "

	topPriority to: 1 by: -1 do:[:priority | | list |
		list := quiescentProcessLists at: priority.
		list do: [:proc | 
			aBlock value: proc
		].
	].
