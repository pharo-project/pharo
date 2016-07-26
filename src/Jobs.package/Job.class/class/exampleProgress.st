examples
exampleProgress

	[:job | job title: 'Let us get started'.
		1 to: 10 do: [:each | 
			job 
				progress: (0.1 * each); 
				title: 'Youpi ', each printString.
			(Delay forMilliseconds: 100) wait. 
			] ]  asJob run