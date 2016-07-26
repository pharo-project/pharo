accessing
acceptEntityDescription: entity
	"Take over the content type and length from entity"
	
	entity 
		ifNotNil: [
			self
				contentType: entity contentType;
				contentLength: entity contentLength ]
		ifNil: [
			self 
				clearContentType; 
				clearContentLength ] 