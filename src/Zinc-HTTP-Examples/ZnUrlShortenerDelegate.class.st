"
I am ZnUrlShortenerDelegate, a web app/service that implements URL Shortening.

When invoked with no arguments, I show a small web page with a form where you can enter a URL.
When you click the 'Shorten' button, a new URL is returned as text.

This short URL consists of just a key (a base 36 integer of 6 digits) in its path.
This key is stored in my database (class side) and linked to the original URL.

When invoked with the key of a shortened URL as path, 
I reply with a redirect that opens the original URL.

It is also possible to access me programmatically (see below, see unit tests).

Usage

 ZnServer startDefaultOn: 1701.
 ZnServer default delegate: ZnUrlShortenerDelegate new.

 ZnClient new url: ZnServer default url; queryAt: #url put: 'http://stfx.eu'; get.

 ZnServer stopDefault.

Thanks to Cédrick Béler for the first implementation of this example.

Part of Zinc HTTP Components
"
Class {
	#name : #ZnUrlShortenerDelegate,
	#superclass : #Object,
	#classVars : [
		'Database'
	],
	#category : #'Zinc-HTTP-Examples'
}

{ #category : #accessing }
ZnUrlShortenerDelegate class >> database [
	^ Database ifNil: [ Database := Dictionary new ]
]

{ #category : #utilities }
ZnUrlShortenerDelegate class >> reset [
	Database := nil
]

{ #category : #utilities }
ZnUrlShortenerDelegate class >> shorten: urlOrString [
	"Shorten urlOrString, store it in our database and return the key"

	"self shorten: 'https://pharo.org'."

	| key url |
	url := urlOrString asUrl.
	key := MD5 hashMessage: url printString.
	key := (key asInteger \\ (36 ** 6)) printStringBase: 36.
	self database at: key put: url.
	^ key
]

{ #category : #'request handling' }
ZnUrlShortenerDelegate >> handleGetRequest: request [
	^ request uri lastPathSegment
			ifNotNil: [ :key |
				self class database
					at: key
					ifPresent: [ :url | ZnResponse redirect: url ]
					ifAbsent: [ ZnResponse notFound: request uri ] ]
			ifNil: [
				(request uri queryAt: #url ifAbsent: [ nil ])
					ifNil: [ ZnResponse ok: (ZnEntity html: self welcomeHTML) ]
					ifNotNil: [ :url | | key shortUrl |
						key := self class shorten: url.
						shortUrl := request server url / key.
						ZnResponse ok: (ZnEntity text: shortUrl printString) ] ]
]

{ #category : #'request handling' }
ZnUrlShortenerDelegate >> handleRequest: request [
	^ request method = #GET
		ifTrue: [ self handleGetRequest: request ]
		ifFalse: [ ZnResponse methodNotAllowed: request ]
]

{ #category : #accessing }
ZnUrlShortenerDelegate >> welcomeHTML [
	^ ZnHtmlOutputStream streamContents: [ :html |
		html page: 'Zn URL Shortener' do: [
			html
				tag: #form
				attributes: #(action '/' 'accept-charset' 'utf-8' method GET)
				do: [
					html
						str: 'URL'; space;
						tag: #input attributes: #(type input name url value 'https://pharo.org' size 100); space;
						tag: #input attributes: #(type submit value 'Shorten') ] ] ]
]
