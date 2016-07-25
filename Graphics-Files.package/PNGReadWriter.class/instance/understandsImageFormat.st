accessing
understandsImageFormat
	#(137 80 78 71 13 10 26 10) do: [ :byte |
		stream next = byte ifFalse: [^ false]].
	^ true
