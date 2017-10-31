private
initializeStream

	self closeStream.

	date := Date today.	
	fileStream := FileStream fileNamed: self datedFileName