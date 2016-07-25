accessing
localUrl
	"Return a ZnUrl to access me."
	
	^ ZnUrl new
		scheme: self scheme;
		host: NetNameResolver loopBackName;
		port: self port;
		yourself