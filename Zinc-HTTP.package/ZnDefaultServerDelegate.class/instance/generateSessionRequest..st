private
generateSessionRequest: session
	"Generate an HTML page using a session"
	
	^ ZnHtmlOutputStream streamContents: [ :html |
		html page: 'Session' do: [ 
			html tag: #p do: [ html << 'I am using '; str: session asString ].
			html tag: #p do: [ html << 'Current session id is ' << session id ].
			html tag: #p do: [ html << 'Session hit count ' << (session attributeAt: #hitCount) ] ] ]