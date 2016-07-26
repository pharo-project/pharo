evaluating
valueWithInterval: aDelay
	"Executes the block every x milliseconds specified in arguments. Answers the process, so you can terminate it"
	
	^ [ [ self value. aDelay wait. ] repeat ] 
		forkAt: Processor userBackgroundPriority 
		named: (String streamContents: [ :s |
				s 
					<< 'every ';
					print: aDelay;
					<<' do: ';
					print: self ] )