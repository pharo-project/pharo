converting
asAthensPaintOn: anAthensCanvas

	| surface |

	 surface := AthensCairoSurface createFromFile: fileName ifFailed: [ :status | 

		status =  CAIRO_STATUS_READ_ERROR ifTrue: [ 
			self error: 'Cairo couldnt read the file named ', fileName ].
		status = CAIRO_STATUS_FILE_NOT_FOUND ifTrue: [
			self error: 'Cairo cant find the file named ', fileName ].

		^ self error: 'Cannot create surface. Cairo status code: ', status asString 
	].
	
	^ surface asAthensPaintOn: anAthensCanvas.
 