I am ZnSecureServer, an implementation of an HTTPS server.
I am a ZnMultiThreadedServer

(ZnSecureServer on: 1443)
	certificate: '/home/sven/ssl/key-cert.pem';
	logToTranscript;
	start;
	yourself.
	
Disclaimer: this is an experimental proof of concept.