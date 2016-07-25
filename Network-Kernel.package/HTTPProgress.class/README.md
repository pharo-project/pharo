I am HTTPProgress, a notification to show progress when using HTTP.

I include
	- total: The total size of the download/upload (if known)
	- amount: The completed amount of the download/upload (if known)

Use #total:, #amount: or #amountLeft: to set the appropriate byte counts to indicate progress.
Use #fraction or #percentage as a value that indicates progress.
Total and amount are optional and can be nil. Test using #isEmpty.

You can try 
	HTTPProgress example.
