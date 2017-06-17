accessing
time
	| time offset |
	time := self commit_time: self.
	offset := self commit_time_offset: self.
	^ LGitTime new
		time: time;
		offset: offset;
		yourself