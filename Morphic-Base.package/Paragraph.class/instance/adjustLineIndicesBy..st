private
adjustLineIndicesBy: delta
	firstCharacterIndex := firstCharacterIndex + delta.
	lines do: [:line | line slide: delta].
