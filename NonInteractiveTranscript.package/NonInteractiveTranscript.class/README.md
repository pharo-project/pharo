I am NonInteractiveTranscript, a replacement for Transcript, writing everything to a file or stdout. I implement TTranscript.

This is useful when running headless.

	NonInteractiveTranscript file install.
	
To connect to the output stream of the virtual machine process choose stdout.

	NonInteractiveTranscript stdout install.

or 

	NonInteractiveTranscript stderr install
