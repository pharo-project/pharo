copying
copyWithTally: hitCount
	^ (QSystemTally new class: class method: method) bumpBy: hitCount