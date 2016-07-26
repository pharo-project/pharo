asserting
assertNoChanges
	"Assert that the current patch is empty.
	
	Browse the patch with:
	
	((PSMCPatchMorph forPatch: EpMiniMonticello current workingPatch) newWindow) open
	
	"

	self assert: EpMiniMonticelloResource current workingPatch isEmpty