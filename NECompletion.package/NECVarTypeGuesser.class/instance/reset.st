private
reset
	contextCount > 0 ifTrue:[^self].
	types reset.
	hasSend := false.