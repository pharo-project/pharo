private
resizeInBuffer: newSize
	"Resize the inBuffer by recreating it.
	This also has the effect of getting rid of
	dead data above inNextToWrite.
	<newSize> must >= inBuffer size!"

	inBuffer := (self streamBuffer: newSize)
					replaceFrom: 1 to: inNextToWrite - 1 with: inBuffer startingAt: 1