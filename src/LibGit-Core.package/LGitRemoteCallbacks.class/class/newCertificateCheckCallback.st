private
newCertificateCheckCallback
	self flag: #fixMe. "I need to skip the verification of certificate because on macOS, 
	libgit2 has problems to be compiled with Secure framework, and if compiled with 
	openssl instead, openssl for macOS does not checks agains the certificates installed 
	at S.O., so it will fail almost always. This is, of course, a workaround... eventually 
	libgit2 will work :P"
	^ LGitTransportCertificateCheckCallback on: [ :cert :value :host :payload  |
		1 ]