truncation
truncated
	"Answer a date and time to the nearest preceding whole second"
	
	^ self species basicNew ticks: { julianDayNumber. seconds. 0 } offset: offset