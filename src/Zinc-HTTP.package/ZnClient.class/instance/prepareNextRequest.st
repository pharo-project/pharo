private protocol
prepareNextRequest
	"The request could be modified destructively in the next run,
	but the request can also be part of a log event, hence copy it"
	request := request copy	
