examples
exampleOutputs

	| transcript |

	transcript := TranscriptStream new.

	transcript 
	addOutput: (TranscriptFileOutput forFileNamed:'example.txt').

	transcript open. "adds a TranscriptRubricOutput"

	transcript
	show: 'HelloWorld';
	close.