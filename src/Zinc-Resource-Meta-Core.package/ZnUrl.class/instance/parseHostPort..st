parsing
parseHostPort: stream
	| hostString portNumber |
	self isFile
		ifTrue: [ 
			(hostString := stream upTo: $/) isEmpty
				ifFalse: [ self host: (self decodePercent: hostString) ] ]
		ifFalse: [ 
			(hostString := stream upTo: $:) isEmpty
				ifFalse: [ self host: (self decodePercent: hostString) ].
			stream atEnd
				ifFalse: [
					portNumber := Integer readFrom: stream ifFail: [ ZnPortNotANumber signal ].
					(portNumber between: 1 and: 65535) ifFalse: [ DomainError signalFrom: 1 to: 65535 ].
					self port: portNumber ] ]