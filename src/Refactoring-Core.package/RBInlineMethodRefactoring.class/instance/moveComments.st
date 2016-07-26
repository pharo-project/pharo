transforming
moveComments
	inlineParseTree nodesDo: 
			[:each | 
			each 
				comments: (each comments collect:
							[:aComment | 
							| start source |
							source := sourceParseTree source.
							start := source size + 1.
							source := source 
										, (inlineParseTree source copyFrom: aComment start to: aComment stop).
							sourceParseTree source: source.
							RBComment with: aComment contents at: start])]