pre-post-load-do-its
initializeForPharo
	"There is a current bug in Pharo 5.0 with the default delaySchedulerClass. See: https://pharo.fogbugz.com/f/cases/18359/Problem-with-DelayExperimentalSpinScheduler-and-delay.
	As a workaround, we rollback to use the millisecond scheduler that apparently does not has this bug. "
	Delay delaySchedulerClass: DelayMillisecondScheduler.