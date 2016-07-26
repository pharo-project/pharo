private
handleRecursionErrorFor: anExample
	^ (GTExampleRecursionDetected new 
		example: self example;
		resolved: processing;
		next: anExample;
		yourself) signal