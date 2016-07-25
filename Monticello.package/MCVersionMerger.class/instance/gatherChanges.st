actions
gatherChanges
	records do: [:ea | merger addBaseSnapshot: ea packageSnapshot].
	records do: [:ea | merger applyPatch: ea mergePatch].