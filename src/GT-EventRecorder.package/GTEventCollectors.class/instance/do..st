enumerating
do: oneArgBlock
	mutex critical: [ collectors do: oneArgBlock ]