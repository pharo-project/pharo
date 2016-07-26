text
scriptDefinitionString
	| defs |
	defs := items select: [:ea | ea isScriptDefinition].
	defs isEmpty ifTrue: [^'(package defines no scripts)'].
	
	^ String streamContents: [:stream | 
		defs asArray sort 
			do: [:ea | stream nextPutAll: '---------- package ';
					nextPutAll: ea scriptSelector;
					nextPutAll: ' ----------'; cr;
					nextPutAll: ea script; cr]
			separatedBy: [stream cr]].