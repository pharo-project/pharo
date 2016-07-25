initialization
initialize
	super initialize.
	self matcher 
		matches: '`{:node | node isMessage and: [node receiver isBlock and: [node parent isCascade not and: [#(#value #value: #value:value: #value:value:value: #valueWithArguments:) includes: node selector]]]}'
		do: [ :node :answer | node ]