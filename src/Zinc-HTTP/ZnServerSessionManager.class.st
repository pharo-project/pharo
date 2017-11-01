"
I am ZnServerSessionManager.
I manage a collection of ZnServerSession on behalf of a ZnServer.

Session ids are stored in cookies.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnServerSessionManager,
	#superclass : #Object,
	#instVars : [
		'sessions'
	],
	#category : 'Zinc-HTTP-Support'
}

{ #category : #accessing }
ZnServerSessionManager >> cleanupInvalidSessions [
	"Remove all expired sessions"
	
	sessions keysAndValuesRemove: [ :key :value | value isValid not ]
]

{ #category : #accessing }
ZnServerSessionManager >> cookieName [
	^ 'session-id'
]

{ #category : #initialization }
ZnServerSessionManager >> initialize [
	super initialize.
	sessions := Dictionary new
]

{ #category : #private }
ZnServerSessionManager >> newSessionForRequest: request [
	| id session |
	id := self newSessionId.
	session := ZnServerSession new.
	session id: id.
	sessions at: id put: session.
	^ session
]

{ #category : #private }
ZnServerSessionManager >> newSessionId [
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
]

{ #category : #accessing }
ZnServerSessionManager >> removeSessionWithId: id [
	"Remove the session with id.
	If there is no such session, do nothing"
	
	^ sessions removeKey: id ifAbsent: [ nil ]
]

{ #category : #accessing }
ZnServerSessionManager >> sessionFor: request [
	"Bind an existing session to request or create a new session"

	| id |
	id := self sessionIdFromRequest: request.
	sessions
		at: id
		ifPresent: [ :session | 
			session isValid
				ifTrue: [ ^ session touch; yourself ]
				ifFalse: [ sessions removeKey: id ] ].
	^ self newSessionForRequest: request
]

{ #category : #private }
ZnServerSessionManager >> sessionIdFromRequest: request [
	| cookie |
	cookie := request cookies 
		detect: [ :each | each name = self cookieName ] 
		ifNone: [ ^ nil ].
	^ cookie value
]

{ #category : #accessing }
ZnServerSessionManager >> setSession: session in: response [
	"Bind session in response"

	| cookie |
	session ifNil: [ ^ self ].
	(cookie := ZnCookie name: self cookieName value: session id)
		path: '/'.
	response addCookie: cookie
]
