public
checkImageInSyncWithUpdate
	"most of the time you want to make sure that your image is in sync with the latest item of the update.list
	but in some cases you do not to get forced to have this behavior."
	
	^ CheckImageInSyncWithUpdate ifNil: [CheckImageInSyncWithUpdate := true]