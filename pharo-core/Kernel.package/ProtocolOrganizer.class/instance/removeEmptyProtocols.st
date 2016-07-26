protocol - removing
removeEmptyProtocols
	
	(self protocols select: [:e | e isEmpty and: [ e canBeRemoved ] ]) 
		do: [:p | self removeProtocol: p ]