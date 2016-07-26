SystemReporter offers a window where information about the system is gathered. This can be easily copied to the clipboard and be attached to a bug report for better identification of the context the bug occured in.

To extend the SystemReporter:
	- add a method
		reportXYZ: aStream
	  to the reporting category
	- insert a line
		add: #XYZ method: #reportXYZ
	  to the initialize method
