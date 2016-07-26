splitjoin
joinUsing: joiner last: last 
	"#(1 2 3 4) joinUsing: ', ' last: 'and'. => '1, 2, 3 and 4"
	(self size = 0) ifTrue: [  ^ '' ].
	(self size = 1) ifTrue: [  ^ self first asString ].
	^ last join: (Array
				with: (joiner join: self allButLast)
				with: self last)