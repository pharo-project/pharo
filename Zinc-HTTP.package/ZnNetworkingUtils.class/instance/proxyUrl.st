proxy
proxyUrl
	"System settings do currently not support HTTPS proxies."

	^ ZnUrl new
		scheme: #http;
		host: self httpProxyServer;
		port: self httpProxyPort;
		yourself