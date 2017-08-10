private - uffi
system: command

	^ self ffiCall: #(#int #system #(#char #* #command)) module: LibC