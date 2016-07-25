stepping
adjustWakeupTimes: now
	"Fix the wakeup times in my step list. This is necessary when this world has been restarted after a pause, say because some other view had control, after a snapshot, or because the millisecond clock has wrapped around. (The latter is a rare occurence with a 32-bit clock!)"
	| deltaTime |
	deltaTime := now - lastStepTime.
	stepList do:[:entry| entry scheduledTime: entry scheduledTime + deltaTime].
	lastStepTime := now.
