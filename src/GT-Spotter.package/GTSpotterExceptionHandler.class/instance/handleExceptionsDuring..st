handling
handleExceptionsDuring: aBlock
	[ GTCurrentSpotterExceptionHandler use: self during: aBlock ] 
		on: self 
		do: [ :exception | 
			[ self handleException: exception ] " handle any exception during aBlock "
				on: Error
				do: [ :fatalException | self handleFatalException: fatalException ] ] " handle exception caused while handling an exception "