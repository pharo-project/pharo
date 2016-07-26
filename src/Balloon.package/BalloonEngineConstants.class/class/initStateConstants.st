pool definition
initStateConstants
	"Initialize the state Constants"

	GEStateUnlocked := 0.			"Buffer is unlocked and can be modified as wanted"
	GEStateAddingFromGET := 1.	"Adding edges from the GET"
	GEStateWaitingForEdge := 2.	"Waiting for edges added to GET"
	GEStateScanningAET := 3.		"Scanning the active edge table"
	GEStateWaitingForFill := 4.		"Waiting for a fill to mix in during AET scan"
	GEStateBlitBuffer := 5.			"Blt the current scan line"
	GEStateUpdateEdges := 6.		"Update edges to next scan line"
	GEStateWaitingChange := 7.	"Waiting for a changed edge"
	GEStateCompleted := 8.			"Rendering completed"

	"Error constants"
	GErrorNoMoreSpace := 1.		"No more space in collection"
	GErrorBadState := 2.			"Tried to call a primitive while engine in bad state"
	GErrorNeedFlush := 3.			"Tried to call a primitive that requires flushing before"

	"Incremental error constants"
	GErrorGETEntry := 4.			"Unknown entry in GET"
	GErrorFillEntry := 5.			"Unknown FILL encountered"
	GErrorAETEntry := 6.			"Unknown entry in AET"
