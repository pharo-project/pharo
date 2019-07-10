"
I am ZnDefaultServerDelegate.
I function as a delegate for ZnServer, implementing #handleRequest:

I implement responses to the following prefixes:

/echo - an echo text of request information for debugging purposes
/dw-bench - a dynamic html page for benchmarking purposes
/unicode - a unicode test page
/random - a random string (/random/32 for a specific size)
/bytes - bytes according to a pattern (/bytes/32 for a specific size)
/favicon.ico - a Zn favicon
/status - a server status page
/error - force server errors
/session - a simpler session based counter
/help - lists all page prefixes
/ - an html welcome page

Without any matches, I respond with a page not found.

I can be reused without any default handlers mapped using #empty and configured using #map:to: where the second argument can be a block.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnDefaultServerDelegate,
	#superclass : #Object,
	#instVars : [
		'prefixMap',
		'byteArray'
	],
	#category : #'Zinc-HTTP-Client-Server'
}

{ #category : #'instance creation' }
ZnDefaultServerDelegate class >> empty [
	^ self basicNew
]

{ #category : #'instance creation' }
ZnDefaultServerDelegate class >> new [
	^ super new initialize
]

{ #category : #responses }
ZnDefaultServerDelegate >> bytes: request [
	"Answer a number of bytes according to a pattern as an application/octet-stream.
	Optionally take the size from the URI, as in /bytes/32 for 32 bytes, default to 64 bytes.
	The main goal is to be efficient, esp. in benchmarks, that's why we cache (non-thread-safe)"
	
	| count |
	count := Integer readFrom: request uri lastPathSegment ifFail: [ 64 ].
	(byteArray isNil or: [ byteArray size ~= count ]) 
		ifTrue: [
			byteArray := ByteArray new: count.
			1 to: count - 1 do: [ :each | 
				byteArray at: each put: (#(65 66 67 68) at: (each \\ 4 + 1)) ].
			byteArray at: count put: 10 ].
	^ ZnResponse ok: (ZnEntity bytes: byteArray)
]

{ #category : #responses }
ZnDefaultServerDelegate >> dwbench: request [
	"Reply with the dynamic DW-Bench HTML page"

	^ ZnResponse ok: (ZnEntity html: self generateDWBench)
]

{ #category : #responses }
ZnDefaultServerDelegate >> echoRequest: request [	
	"Echo request by generating a plain text response useful for debugging purposes"
	
	| delay info entity |
	delay := Integer readFrom: (request uri queryAt: 'delay' ifAbsent: [ '' ]) ifFail: [ 0 ].
	delay > 0 ifTrue: [ (Delay forSeconds: delay) wait ].  
	info := self generateEchoRequestString: request.
	entity := ZnEntity textCRLF: info.
	^ ZnResponse ok: entity
]

{ #category : #responses }
ZnDefaultServerDelegate >> errorResponse: request [
	"Here we artificially signal an Error at the Smalltalk level,
	optionally with a user defined message."
	
	| message |
	message := request uri queryAt: 'message' ifAbsent: [ 'An artificial error' ].
	Error signal: message
]

{ #category : #responses }
ZnDefaultServerDelegate >> favicon: request [
	"Answer a nice favicon for browsers to display"

	| entity |
	entity := ZnEntity with: ZnConstants faviconBytes type: 'image/vnd.microsoft.icon'. 
	^ ZnResponse ok: entity
]

{ #category : #responses }
ZnDefaultServerDelegate >> formTest1: request [
	| input page |
	input := request uri queryAt: #input ifAbsent: [ 'input' ].
	page := ZnHtmlOutputStream streamContents: [ :html |
		html page: 'Form Test 1' do: [ 
			html 
				tag: #form 
				attributes: #(action 'form-test-1' 'accept-charset' 'utf-8' method GET) 
				do: [ 
					html 
						str: 'Input'; space;
						tag: #input attributes: { #type. #input. #name. #input. #value. input }; space;
						tag: #input attributes: #(type submit); 
						str: 'for input'; space; tag: #em with: input ] ] ].
	^ ZnResponse ok: (ZnEntity html: page)
]

{ #category : #responses }
ZnDefaultServerDelegate >> formTest2: request [
	| input page |
	input := 'input'.
	(request hasEntity and: [ request contentType matches: ZnMimeType applicationFormUrlEncoded  ])
		ifTrue: [ input := request entity at: #input ifAbsent: [ 'input' ] ].
	page := ZnHtmlOutputStream streamContents: [ :html |
		html page: 'Form Test 2' do: [ 
			html 
				tag: #form 
				attributes: #(action 'form-test-2' 'accept-charset' 'utf-8' method POST) 
				do: [ 
					html 
						str: 'Input'; space;
						tag: #input attributes: { #type. #input. #name. #input. #value. input }; space;
						tag: #input attributes: #(type submit); 
						str: 'for input'; space; tag: #em with: input ] ] ].
	^ ZnResponse ok: (ZnEntity html: page)
]

{ #category : #responses }
ZnDefaultServerDelegate >> formTest3: request [
	| contents filename contentType page |
	contents := filename := contentType := ''.
	(request hasEntity and: [ request contentType matches: ZnMimeType multiPartFormData ])
		ifTrue: [ 
			(request entity partNamed: #file ifNone: [ nil ]) 
				ifNotNil: [ :part |
					filename := part fileName.
					contents := part contents.
					contentType := part  contentType.
					contentType isBinary ifTrue: [ contents := contents hex ] ] ].
	page := ZnHtmlOutputStream streamContents: [ :html |
		html page: 'Form Test 3' do: [ 
			html 
				tag: #form 
				attributes: #(action 'form-test-3' 'accept-charset' 'utf-8' 
									enctype 'multipart/form-data' method POST) 
				do: [ 
					html 
						str: 'File'; space;
						tag: #input attributes: #(type file name file); space;
						tag: #input attributes: #(type submit) ];
				tag: #p do: [ html str: 'filename = '; str: filename ];
				tag: #p do: [ html str: 'content-type = '; str: contentType asString ];
				tag: #p do: [ html str: 'contents = '; str: contents asString ] ] ].
	^ ZnResponse ok: (ZnEntity html: page)
]

{ #category : #private }
ZnDefaultServerDelegate >> generateDWBench [
	"Generate the dynamic DW-Bench HTML page.
	Although the date/time is variable, the page size is constant."
	
	^ ZnHtmlOutputStream streamContents: [ :html |
		html page: 'DW-Bench Dynamic' do: [ 
			html tag: #table attributes: #(border 1) do: [
				1 to: 25 do: [ :row |
					html tag: #tr do: [
						1 to: 25 do: [ :col |
							html tag: #td do: [ html print: row * col ] ] ] ] ].
			html tag: #p do: [ 
				html << Date today yyyymmdd; space; << Time now print24 ] ] ]
]

{ #category : #private }
ZnDefaultServerDelegate >> generateEchoRequestString: request [
	"Generate a string echoing the request"
	
	^ String streamContents: [ :stream |
		stream nextPutAll: 'This is Zinc HTTP Components echoing your request !'; crlf.
		stream nextPutAll: 'Running '; print: request server; crlf.
		stream nextPutAll: request method; nextPutAll: ' request for '.
		request uri printPathQueryFragmentOn: stream.
		stream crlf; nextPutAll: 'with headers'; crlf.
		request headersDo: [ :key :value |
			stream space; nextPutAll: key; nextPutAll: ': '; nextPutAll: value; crlf ].
		request hasEntity ifTrue: [
			stream nextPutAll: ' containing '; print: request entity ] ]
]

{ #category : #private }
ZnDefaultServerDelegate >> generateHelp [
	"Generate an HTML page with links to all pages I support"
	
	^ ZnHtmlOutputStream streamContents: [ :html |
		html page: (self class name, ' Help') do: [  	
			html tag: #h3 with: 'Available Pages'.
			html tag: #ul do: [
				prefixMap keys sorted do: [ :each | 
					html tag: #li do: [ 
						html tag: #a attributes: { #href. each } with: each ] ] ] ] ]
]

{ #category : #private }
ZnDefaultServerDelegate >> generateSessionRequest: session [
	"Generate an HTML page using a session"
	
	^ ZnHtmlOutputStream streamContents: [ :html |
		html page: 'Session' do: [ 
			html tag: #p do: [ html << 'I am using '; str: session asString ].
			html tag: #p do: [ html << 'Current session id is ' << session id ].
			html tag: #p do: [ html << 'Session hit count ' << (session attributeAt: #hitCount) asString ] ] ]
]

{ #category : #private }
ZnDefaultServerDelegate >> generateStatus [
	"Generate an HTML page with the state of the server"
	
	^ ZnHtmlOutputStream streamContents: [ :html |
		html page: 'ZnServer Status' do: [
			html tag: #p do: [ html << 'I am '; print: ZnCurrentServer value ].
			html tag: #p with: self systemVersionInfo.
			html tag: #pre with: self vmStats.
			html tag: #h1 with: 'Processes'.
			html tag: #ol do: [ 
				self processes do: [ :each |
					html tag: #li do: [ 
						html 
							str: each name; space; nextPut: $[; print: each priority; nextPut: $]; 
							tag: #br; str: each printString ] ] ] ] ]
]

{ #category : #private }
ZnDefaultServerDelegate >> generateUnicodeTest [
	"Generate an HTML page containing all Unicode characters with code points between 0 and 16r024F in a nice table"
	
	^ ZnHtmlOutputStream streamContents: [ :html |
		html page: 'Unicode Test Page' do: [ 
			html tag: #table do: [
				0 to: 16r024F by: 8 do: [ :row |
					html tag: #tr do: [
					row to: row + 7 do: [ :each |
						html tag: #td do: [ 
							html 
								nextPutAll: 'U+'; nextPutAll: (each printPaddedWith: $0 to: 4 base: 16);
								nextPut: $=; nextPut: each asCharacter ] ] ] ] ] ] ]
]

{ #category : #private }
ZnDefaultServerDelegate >> generateWelcomePage [
	^ ZnHtmlOutputStream streamContents: [ :html |
		html html5; tag: #html do: [
			html tag: #head do: [
				html tag: #title with: 'Zinc HTTP Components'.
				html tag: #style attributes: #(type 'text/css') with: self welcomePageCss ].
		html tag: #body do: [ 
			html tag: #div id: #logo with: #Zn.
			html tag: #h1 with: 'Zinc HTTP Components'.
			html tag: #p with: self welcomePageGreeting.
			self welcomePageLinksOn: html.
			html tag: #h4 with: 'May the Source be with you!' ] ] ]
]

{ #category : #public }
ZnDefaultServerDelegate >> handleRequest: request [
	| prefix prefixHandler |
	prefix := request uri isSlash
		ifTrue: [ prefixMap at: '/' ifAbsent: [ nil ] ]
		ifFalse: [ request uri firstPathSegment ].
	prefixHandler := prefixMap at: prefix ifAbsent: [ nil ].
	prefixHandler isNil
		ifFalse: [ 
			^ prefixHandler isSymbol
				ifTrue: [ self perform: prefixHandler with: request ]
				ifFalse: [ prefixHandler value: request ] ].
	^ ZnResponse notFound: request uri
]

{ #category : #responses }
ZnDefaultServerDelegate >> help: request [
	"Reply with a dynamic HTML page containing links to all pages I support"

	^ ZnResponse ok: (ZnEntity html: self generateHelp)
	
]

{ #category : #initialization }
ZnDefaultServerDelegate >> initialize [
	self
		map: 'welcome' to: #welcome:;
		map: 'help' to: #help:;
		map: 'echo' to: #echoRequest:;
		map: 'small' to: #small:;
		map: 'dw-bench' to: #dwbench:;
		map: 'favicon.ico' to: #favicon:;
		map: 'random' to: #random:;
		map: 'bytes' to: #bytes:;
		map: 'status' to: #status:;
		map: 'unicode' to: #unicode:;
		map: 'session' to: #sessionRequest:;
		map: 'error' to: #errorResponse:;
		map: 'form-test-1' to: #formTest1:;
		map: 'form-test-2' to: #formTest2:;
		map: 'form-test-3' to: #formTest3:;
		map: '/' to: 'welcome'
]

{ #category : #accessing }
ZnDefaultServerDelegate >> map: prefix to: handler [
	"Arrange for the receiver to dispatch requests matching prefix to handler.
	Handler can be a symbol naming a method in the receiver, 
	or a block accepting the request and producing the response.
	The special prefix '/' is resolved first and can refer to another prefix."
	
	self prefixMap 
		at: prefix
		put: handler
		
]

{ #category : #accessing }
ZnDefaultServerDelegate >> prefixMap [
	^ prefixMap ifNil: [ prefixMap := Dictionary new ]
]

{ #category : #private }
ZnDefaultServerDelegate >> processes [
	^ (Process allSubInstances
		reject: [ :each | each isTerminated or: [ each = Processor activeProcess ] ])
		sorted: [ :a :b | a priority >= b priority ] 
]

{ #category : #responses }
ZnDefaultServerDelegate >> random: request [
	"Answer a text plain UTF-8 encoded string of random hex characters.
	Optionally take the size from the URI, as in /random/32 for 32 characters.
	Default to 64 characters with a newline at the end"
	
	| count hexString |
	count := Integer readFrom: request uri lastPathSegment ifFail: [ 64 ].
	hexString := String new: count streamContents: [ :stream |
		count - 1 timesRepeat: [ stream nextPut: '0123456789ABCDEF' atRandom ].
		stream lf ].
	^ ZnResponse ok: (ZnEntity text: hexString)
]

{ #category : #responses }
ZnDefaultServerDelegate >> sessionRequest: request [	
	"An HTML response that uses a session"
	
	| session hitCount |
	session := request session.
	hitCount := session attributeAt: #hitCount ifAbsent: [ 0 ].
	hitCount := hitCount + 1.
	session attributeAt: #hitCount put: hitCount. 
	^ ZnResponse ok: (ZnEntity html: (self generateSessionRequest: session))
]

{ #category : #responses }
ZnDefaultServerDelegate >> small: request [
	| page |
	page := ZnHtmlOutputStream streamContents: [ :html |
		html page: #Small do: [ 
			html tag: #p with: 'This is a small HTML document' ] ].
	^ ZnResponse ok: (ZnEntity html: page)
]

{ #category : #responses }
ZnDefaultServerDelegate >> status: request [
	"Reply with a dynamic HTML page describing the state of the server"

	^ ZnResponse ok: (ZnEntity html: self generateStatus)
	
]

{ #category : #private }
ZnDefaultServerDelegate >> systemVersionInfo [
	^ String streamContents: [ :stream |
			stream 
				print: SystemVersion current; 
				nextPutAll: ' - '; 
				nextPutAll: ZnConstants defaultServerString ]
	
]

{ #category : #responses }
ZnDefaultServerDelegate >> unicode: request [
	"Answer a Unicode Test HTML page"
	
	^ ZnResponse ok: (ZnEntity html: self generateUnicodeTest)
]

{ #category : #public }
ZnDefaultServerDelegate >> value: request [
	"I implement the generic #value: message as equivalent to #handleRequest:"
	
	^ self handleRequest: request
]

{ #category : #private }
ZnDefaultServerDelegate >> vmStats [
	^ SmalltalkImage current vm statisticsReport
]

{ #category : #responses }
ZnDefaultServerDelegate >> welcome: request [
	"Answer a nice HTML welcome page"
	
	| page entity |
	page := self generateWelcomePage.
	entity := ZnEntity html: page. 
	^ ZnResponse ok: entity
]

{ #category : #private }
ZnDefaultServerDelegate >> welcomePageCss [
	^ Character space join: 
	'body { color: black; background: white; width: 900px; font-family: Verdana, Arial, Helvetica, sans-serif; font-size: 13px }
p { width: 600px; padding: 0 20px 10px 20px }
ul, ol { width: 600px; padding: 0 5px 5px 30px } 
#logo { color: orange; font-family: Helvetica, sans-serif; font-weight: bold; font-size: 100px }' lines
]

{ #category : #private }
ZnDefaultServerDelegate >> welcomePageGreeting [
	^ 'Welcome to Zinc HTTP Components, a modern, open-source Smalltalk framework to deal with the HTTP networking protocol.'
]

{ #category : #private }
ZnDefaultServerDelegate >> welcomePageLinksOn: html [
	html tag: #ul do: [ 
		html tag: #li do: [ 
			html 
				str: 'Project homepage'; space;  
				tag: #a attributes: #(href 'http://zn.stfx.eu') with: 'http://zn.stfx.eu' ].
		html tag: #li do: [ 
			html 
				str: 'Locally available pages'; space;  
				tag: #a attributes: #(href '/help') with: '/help' ] ]
]
