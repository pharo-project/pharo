Class {
	#name : 'ZnProxyServerDelegateTest',
	#superclass : 'TestCase',
	#category : 'Zinc-HTTP-Examples',
	#package : 'Zinc-HTTP-Examples'
}

{ #category : 'testing' }
ZnProxyServerDelegateTest >> testEcho [
	| proxiedServer proxyServer proxyDelegate client  |
	proxiedServer := ZnServer startOn: 8080.
	proxyServer := ZnServer on: 9090.
	proxyDelegate := ZnProxyServerDelegate new
		server: proxyServer;
		proxyBlock: [ :request | 
			request url: (request url port: 8080) ].
	proxyServer
		delegate: proxyDelegate;
		start.
	client := ZnClient new.
	client get: proxyServer url / #echo.
	self assert: client isSuccess.
	self assert: (client contents includesSubstring: 'echoing').
	self assert: (client contents includesSubstring: 'running 8080').
	proxiedServer stop.
	proxyServer stop
]
