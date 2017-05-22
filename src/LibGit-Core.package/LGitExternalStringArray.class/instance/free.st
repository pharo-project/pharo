initialize-release
free
	1 to: self size do: [ :index | 
		(self primAt: index) free ].
	
	super free