initialization
initialize
	super initialize.
	self matcher 
		matches: '`{:node | node isMessage and: [node isUsed not and: [#(#= #== #~= #~~ #< #> #<= #>=) includes: node selector]]}'
		do: [ :node :answer | node ]