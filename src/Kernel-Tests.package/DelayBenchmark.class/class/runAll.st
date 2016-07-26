action
runAll
	"DelayBenchmark runAll"
	Transcript clear.
	DelayScheduler allSubclasses do: [ :delayScheudlerClass | self new bench: delayScheudlerClass ].