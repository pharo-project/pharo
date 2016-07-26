instance creation
new
	"New instances of ProcessorScheduler should not be created."

	self error:
'New ProcessSchedulers should not be created since
the integrity of the system depends on a unique scheduler'