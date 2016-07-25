private
newSessionId
	"Create a new session id.
	If the server has a route, append it at the end with a dot as separator."
	
	^ ZnCurrentServer value route
		ifNil: [ UUID new asString36 ]
		ifNotNil: [ :route | 
			String
				streamContents: [ :stream | 
					stream
						<< UUID new asString36;
						nextPut: $.;
						<< route ] ]