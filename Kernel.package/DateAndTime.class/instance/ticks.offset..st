private
ticks: ticks offset: utcOffset
	"ticks is {julianDayNumber. secondCount. nanoSeconds}"

	self setJdn: (ticks at: 1) seconds: (ticks at: 2) nano: (ticks at: 3) offset: utcOffset