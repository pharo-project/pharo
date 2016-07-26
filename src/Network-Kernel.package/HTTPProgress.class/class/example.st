examples
example
	"self example"
	
 	UIManager default informUserDuring: [ :bar |
		bar label: 'Transfer Demo...'.
		1 second asDelay wait.
		[ 1 to: 20 do: [ :each |
			HTTPProgress signal: 'Transferring...' amount: each * 1024 total: 20*1024.
			(Delay forMilliseconds: 100) wait ] ]
			on: HTTPProgress 
			do: [ :progress | 
				bar label: progress printString.
				progress isEmpty ifFalse: [ bar current: progress percentage ]. 
				progress resume ] ] 