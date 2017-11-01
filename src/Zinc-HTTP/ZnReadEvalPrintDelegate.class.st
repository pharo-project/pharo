"
I am ZnReadEvalPrintDelegate, I export a REPL Web Service.
You can use this service to work with a headless image.

	ZnReadEvalPrintDelegate startInServerOn: 1701.
	
	ZnClient new
		url: 'http://localhost:1701/repl';
		contents: '42 factorial';
		post.
	
Web Service API:

	POST /repl <some Smalltalk code>
	 
Here is an example terminal session:

$ curl http://localhost:1701/repl
# Pharo Smalltalk REPL. POST expressions to evaluate
# Here is one way (type ctrl-d to end input)
curl -X POST -H'Content-Type:text/plain' --data-binary @- http://localhost:1701/repl

$ curl -X POST -H'Content-Type:text/plain' -d '42 factorial' http://localhost:1701/repl
1405006117752879898543142606244511569936384000000000

$ curl -X POST -H'Content-Type:text/plain' --data-binary @- http://localhost:1701/repl
{ (1 to: 10) sum. (1 to: 10) average }
{55. (11/2)}

There is error handling as well:

$ curl -X POST -H'Content-Type:text/plain' --data-binary @- http://localhost:1701/repl
1 plus: 2
MessageNotUnderstood: SmallInteger>>plus:
SmallInteger(Object)>>doesNotUnderstand: #plus:
	Receiver: 1
	Arguments and temporary variables: 
		aMessage: 	plus: 2
		exception: 	MessageNotUnderstood: SmallInteger>>plus:
		resumeValue: 	nil
	Receiver's instance variables: 
1
UndefinedObject>>DoIt
Compiler>>evaluate:in:to:notifying:ifFail:logged:
Compiler>>evaluate:in:to:notifying:ifFail:
Compiler>>evaluate:in:to:
ZnReadEvalPrintDelegate>>evaluate: in Block: [| result |...
BlockClosure>>on:do:
ZnReadEvalPrintDelegate>>evaluate: in Block: [:out | [| result |...
String class(SequenceableCollection class)>>new:streamContents:

WARNING: never open this service beyond your local network !
This service gives you absolute control over and access to everything in your image. 
For example, the following will kill your image:

$ curl -X POST -H'Content-Type:text/plain' -d 'Smalltalk quitPrimitive' http://localhost:1701/repl

Part of Zinc HTTP Components. 
"
Class {
	#name : #ZnReadEvalPrintDelegate,
	#superclass : #Object,
	#category : 'Zinc-HTTP-Client-Server'
}

{ #category : #public }
ZnReadEvalPrintDelegate class >> startInServerOn: port [
	"Start a new server bound to port on the local network running a REPL web service"

	"self startInServerOn: 1701"
	
	 ^ (ZnServer on: port)
			bindingAddress: NetNameResolver loopBackAddress;
			delegate: self new;
			start;
			yourself
]

{ #category : #private }
ZnReadEvalPrintDelegate >> evaluate: string [
	| output |
	output := String streamContents: [ :out | 
		[ 	| compiler result |
			compiler := [ self class compiler ] 
								on: MessageNotUnderstood 
								do: [ Smalltalk at: #Compiler "for Pharo 2.0" ].		
			result := compiler evaluate: string. 
			out print: result; cr ]
			on: Error
			do: [ :exception |
				out print: exception; cr.
				exception signalerContext printDetails: out.
				exception signalerContext sender debugStack: 8 on: out ].
		out cr ].
	^ String lf join: output lines
]

{ #category : #'request handling' }
ZnReadEvalPrintDelegate >> handleGetRequest: request [
	^ ZnResponse ok: (ZnEntity text: (self helpText format: { request server port })) 
]

{ #category : #'request handling' }
ZnReadEvalPrintDelegate >> handlePostRequest: request [
	^ ZnResponse ok: (ZnEntity text: (self evaluate: request entity contents))
]

{ #category : #public }
ZnReadEvalPrintDelegate >> handleRequest: request [
	request uri firstPathSegment = #repl
		ifTrue: [ 
			request method = #GET
				ifTrue: [ 
					^ self handleGetRequest: request ].
			(request method = #POST and: [ request hasEntity and: [ request contentType isBinary not ] ])
				ifTrue: [ 
					^ self handlePostRequest: request ] ].
	^ ZnResponse notFound: request uri
]

{ #category : #private }
ZnReadEvalPrintDelegate >> helpText [
	^ '# Pharo Smalltalk REPL. POST expressions to evaluate\# Here is one way (type ctrl-d to end input)\curl -X POST -H''Content-Type:text/plain'' --data-binary @- http://localhost:{1}/repl\' 
			withCRs 
				replaceAll: Character cr with: Character lf; 
				yourself
]

{ #category : #public }
ZnReadEvalPrintDelegate >> value: request [
	"I implement the generic #value: message as equivalent to #handleRequest:"
	
	^ self handleRequest: request
]
