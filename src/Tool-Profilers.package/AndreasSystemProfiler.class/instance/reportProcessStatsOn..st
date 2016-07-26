reporting
reportProcessStatsOn: str
	| totalSwitches pageOverflows pageDivorces actualSwitches |
	vmStats size >= 61 ifFalse: [ ^ self ]. "don't try this on the closure VM"
	totalSwitches := vmStats at: 56.
	actualSwitches := totalSwitches - (2*totalTally). "exclude to/from profiler"
	pageOverflows := vmStats at: 60.
	pageDivorces := vmStats at: 61.

	str cr.
	str nextPutAll: '**Processes**'; cr.
	str tab; nextPutAll: 'Total process switches: ', totalSwitches printString; cr.
	str tab; nextPutAll: 'Without Profiler: ', actualSwitches printString; cr.
	str tab; nextPutAll: 'Stack page overflows: ', pageOverflows printString; cr.
	str tab; nextPutAll: 'Stack page divorces: ', pageDivorces printString; cr.
