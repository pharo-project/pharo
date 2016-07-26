operations
notifyNoCommonAncestorWith: aVersion
	self notify:
'Could not find a common ancestor between (',
aVersion info name,
') and (',
ancestry ancestorString, ').
Proceeding with this merge may cause spurious conflicts.'