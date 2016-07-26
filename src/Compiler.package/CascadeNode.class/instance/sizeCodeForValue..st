code generation
sizeCodeForValue: encoder
	| size |
	size := (receiver sizeCodeForValue: encoder)
			 + (messages size - 1 * (encoder sizeDup + encoder sizePop)).
	messages do: [:aMessage | size := size + (aMessage sizeCodeForValue: encoder)].
	^size