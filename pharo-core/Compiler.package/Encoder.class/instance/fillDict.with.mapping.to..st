initialize-release
fillDict: dict with: nodeClass mapping: keys to: codeArray
	| codeStream |
	codeStream := codeArray readStream.
	keys do: 
		[:key | dict 
				at: key
				put:  (nodeClass new name: key key: key code: codeStream next)]