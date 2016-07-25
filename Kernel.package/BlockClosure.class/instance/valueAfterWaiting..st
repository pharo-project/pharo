evaluating
valueAfterWaiting: aDelay
	"Waits for a delay, then executes the block. Answers the process so you can terminate it"
	
	^ [ aDelay wait. self value ] 
		forkAt: Processor userBackgroundPriority
		named: (String streamContents: [ :s |
				s 
					<< 'After ';
					print: aDelay;
					<<' do: ';
					print: self ] )