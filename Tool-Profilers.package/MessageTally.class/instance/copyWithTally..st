private
copyWithTally: hitCount
	^ (self class new class: class method: method)
		reportOtherProcesses: reportOtherProcesses;
		process: process;
		bump: hitCount